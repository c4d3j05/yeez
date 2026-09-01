{-# LANGUAGE OverloadedStrings #-}

-- | The only module in yeez that talks to amazonka.
--
-- Everything the UI needs from S3 is exposed here as a plain @IO@ action
-- taking an 'Env' plus 'Text' bucket/key arguments, so the UI layer never
-- sees an amazonka type.
--
-- Note on versions: amazonka's generated field and lens names shift
-- between releases. This module targets the amazonka 2.x line
-- (@amazonka-2.0@ / @amazonka-s3-2.0@); on a different resolution you may
-- need small accessor renames, all of which are confined to this file.
module S3.Client
  ( newAwsEnv
  , listAllBuckets
  , listObjectsUnder
  , uploadFile
  , downloadFile
  , deleteKey
  , copyKey
  , createFolderMarker
  ) where

import qualified Amazonka as AWS
import Amazonka (Env)
import qualified Amazonka.S3 as S3
import qualified Amazonka.S3.Lens as S3L
import Control.Monad (void)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString as BS
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro ((&), (.~), (^.))
import Numeric (showHex)

-- | Build an AWS environment using the standard credential chain:
-- environment variables, @~\/.aws\/credentials@, container/instance role.
newAwsEnv :: IO Env
newAwsEnv = AWS.newEnv AWS.discover

-- | All buckets visible to the caller (@ListBuckets@).
listAllBuckets :: Env -> IO [Text]
listAllBuckets env = runResourceT $ do
  rs <- AWS.send env S3.newListBuckets
  pure
    [ S3.fromBucketName (b ^. S3L.bucket_name)
    | b <- fromMaybe [] (rs ^. S3L.listBucketsResponse_buckets)
    ]

-- | List one "directory level" of a bucket: everything directly under
-- @prefix@, with @\/@ as the delimiter.
--
-- Returns @(folderPrefixes, files)@ where each folder prefix keeps its
-- trailing @\/@ and each file is a @(key, sizeInBytes)@ pair. Pagination
-- is handled by amazonka's pager, so listings are not truncated at 1000
-- keys.
listObjectsUnder :: Env -> Text -> Text -> IO ([Text], [(Text, Integer)])
listObjectsUnder env bucket prefix = runResourceT $ do
  pages <-
    C.runConduit $
      AWS.paginate env req C..| CC.sinkList
  let folders =
        [ p
        | page <- pages
        , cp <- fromMaybe [] (page ^. S3L.listObjectsV2Response_commonPrefixes)
        , Just p <- [cp ^. S3L.commonPrefix_prefix]
        ]
      files =
        [ (keyText (o ^. S3L.object_key), o ^. S3L.object_size)
        | page <- pages
        , o <- fromMaybe [] (page ^. S3L.listObjectsV2Response_contents)
        ]
  pure (folders, files)
  where
    req =
      S3.newListObjectsV2 (S3.BucketName bucket)
        & S3L.listObjectsV2_delimiter .~ Just '/'
        & S3L.listObjectsV2_prefix .~ (if T.null prefix then Nothing else Just prefix)

-- | Upload a local file to @bucket\/key@ (@PutObject@).
uploadFile :: Env -> Text -> Text -> FilePath -> IO ()
uploadFile env bucket key path = do
  body <- AWS.chunkedFile AWS.defaultChunkSize path
  runResourceT . void $
    AWS.send env (S3.newPutObject (S3.BucketName bucket) (S3.ObjectKey key) body)

-- | Download @bucket\/key@ to a local path (@GetObject@).
downloadFile :: Env -> Text -> Text -> FilePath -> IO ()
downloadFile env bucket key path = runResourceT $ do
  rs <- AWS.send env (S3.newGetObject (S3.BucketName bucket) (S3.ObjectKey key))
  AWS.sinkBody (rs ^. S3L.getObjectResponse_body) (CC.sinkFile path)

-- | Delete a single key (@DeleteObject@).
deleteKey :: Env -> Text -> Text -> IO ()
deleteKey env bucket key =
  runResourceT . void $
    AWS.send env (S3.newDeleteObject (S3.BucketName bucket) (S3.ObjectKey key))

-- | Server-side copy within one bucket (@CopyObject@). Combined with
-- 'deleteKey' this is how yeez implements rename, since S3 has no
-- native rename.
copyKey :: Env -> Text -> Text -> Text -> IO ()
copyKey env bucket srcKey dstKey =
  runResourceT . void $
    AWS.send env (S3.newCopyObject (S3.BucketName bucket) source (S3.ObjectKey dstKey))
  where
    source = encodeCopySource (bucket <> "/" <> srcKey)

-- | Create the zero-byte, trailing-@\/@ marker object that the AWS console
-- and CLI use to represent an empty folder (@PutObject@).
createFolderMarker :: Env -> Text -> Text -> IO ()
createFolderMarker env bucket key =
  runResourceT . void $
    AWS.send env (S3.newPutObject (S3.BucketName bucket) (S3.ObjectKey key') (AWS.toBody BS.empty))
  where
    key' = if "/" `T.isSuffixOf` key then key else key <> "/"

keyText :: S3.ObjectKey -> Text
keyText (S3.ObjectKey t) = t

-- | Percent-encode a @bucket\/key@ copy source. S3 wants the source
-- URL-encoded, but path separators must survive as-is.
encodeCopySource :: Text -> Text
encodeCopySource = T.concatMap enc
  where
    enc c
      | c `elem` ("/-_.~" :: String) = T.singleton c
      | c >= 'a' && c <= 'z' = T.singleton c
      | c >= 'A' && c <= 'Z' = T.singleton c
      | c >= '0' && c <= '9' = T.singleton c
      | otherwise = T.concat (map octet (utf8Bytes c))
    octet b = T.pack ('%' : pad (showHex b ""))
    pad [d] = ['0', d]
    pad ds = map upper ds
    upper d = if d >= 'a' && d <= 'f' then toEnum (fromEnum d - 32) else d

-- | UTF-8 encode a single character into its bytes.
utf8Bytes :: Char -> [Int]
utf8Bytes c
  | n < 0x80 = [n]
  | n < 0x800 = [0xC0 + shiftR' n 6, cont n 0]
  | n < 0x10000 = [0xE0 + shiftR' n 12, cont n 6, cont n 0]
  | otherwise = [0xF0 + shiftR' n 18, cont n 12, cont n 6, cont n 0]
  where
    n = ord c
    shiftR' x k = x `div` (2 ^ (k :: Int))
    cont x k = 0x80 + (shiftR' x k `mod` 0x40)
