{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  ( ConnParams (..)
  , newAwsEnv
  , newAwsEnvFromParams
  , listAllBuckets
  , ConnCheck (..)
  , checkConnection
  , listObjectsUnder
  , uploadFile
  , downloadFile
  , deleteKey
  , copyKey
  , createFolderMarker
  ) where

import qualified Amazonka as AWS
import Amazonka (Env)
import qualified Amazonka.Auth as Auth
import qualified Amazonka.S3 as S3
import qualified Amazonka.S3.Lens as S3L
import Control.Exception (SomeException, displayException, try)
import Control.Monad (void)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Lens.Micro ((&), (.~), (^.))
import Numeric (showHex)
import Text.Read (readMaybe)

-- | Explicit connection settings collected by the setup wizard.
--
-- 'cpEndpoint', when set, points yeez at a non-AWS, S3-compatible service
-- (MinIO, Cloudflare R2, DigitalOcean Spaces, …). Setting it also flips the
-- addressing style to path-style, which those services generally require.
data ConnParams = ConnParams
  { cpAccessKey :: Text
    -- ^ AWS access key id.
  , cpSecretKey :: Text
    -- ^ AWS secret access key.
  , cpRegion :: Text
    -- ^ Region, e.g. @\"us-east-1\"@.
  , cpEndpoint :: Maybe Text
    -- ^ Optional endpoint URL, e.g. @\"https:\/\/minio.example.com:9000\"@.
  } deriving (Eq, Show)

-- | Build an AWS environment using the standard credential chain:
-- environment variables, @~\/.aws\/credentials@, container/instance role.
newAwsEnv :: IO Env
newAwsEnv = AWS.newEnv AWS.discover

-- | Build an AWS environment from explicit 'ConnParams': static keys, an
-- overridden region and, optionally, a custom S3 endpoint with path-style
-- addressing.
newAwsEnvFromParams :: ConnParams -> IO Env
newAwsEnvFromParams cp = do
  base <-
    AWS.newEnv
      ( pure
          . Auth.fromKeys
              (AWS.AccessKey (encodeUtf8 (cpAccessKey cp)))
              (AWS.SecretKey (encodeUtf8 (cpSecretKey cp)))
      )
  let regioned = base { AWS.region = AWS.Region' (cpRegion cp) }
  pure $ case cpEndpoint cp of
    Nothing -> regioned
    Just url ->
      let (secure, host, port) = parseEndpoint url
       in AWS.overrideService (pathStyle . AWS.setEndpoint secure host port) regioned
  where
    pathStyle svc = svc { AWS.s3AddressingStyle = AWS.S3AddressingStylePath }

-- | Split an endpoint URL into @(secure, host, port)@ for 'AWS.setEndpoint'.
-- Defaults to HTTPS on 443 (or HTTP on 80) when the scheme or port is absent.
parseEndpoint :: Text -> (Bool, ByteString, Int)
parseEndpoint url =
  let (scheme, afterScheme) = case T.breakOn "://" url of
        (s, r) | not (T.null r) -> (s, T.drop 3 r)
        _ -> ("https", url)
      secure = scheme /= "http"
      hostPort = T.takeWhile (/= '/') afterScheme
      (host, portPart) = T.break (== ':') hostPort
      port = case T.stripPrefix ":" portPart >>= (readMaybe . T.unpack) of
        Just n -> n
        Nothing -> if secure then 443 else 80
   in (secure, encodeUtf8 host, port)

-- | All buckets visible to the caller (@ListBuckets@).
listAllBuckets :: Env -> IO [Text]
listAllBuckets env = runResourceT $ do
  rs <- AWS.send env S3.newListBuckets
  pure
    [ S3.fromBucketName (b ^. S3L.bucket_name)
    | b <- fromMaybe [] (rs ^. S3L.listBucketsResponse_buckets)
    ]

-- | The outcome of probing a connection with a @ListBuckets@ call.
data ConnCheck
  = ConnOK
    -- ^ Credentials valid and buckets are listable.
  | ConnDenied
    -- ^ Credentials valid, but @ListBuckets@ is forbidden (a 403 /
    -- @AccessDenied@). This is the normal shape of a bucket-scoped IAM
    -- policy: the connection works, it just cannot enumerate buckets, so the
    -- user must open a bucket by name.
  | ConnFailed Text
    -- ^ Anything else — bad keys, wrong endpoint, network error, …. The text
    -- is a one-line, human-readable summary.
  deriving (Eq, Show)

-- | Probe @env@ with a @ListBuckets@ call and classify the result.
--
-- A 403 / @AccessDenied@ is deliberately treated as a working connection
-- ('ConnDenied') rather than a failure: reaching @AccessDenied@ means AWS
-- authenticated the request and only authorization was refused, so the
-- credentials themselves are good. We match on the S3 error code
-- (@AccessDenied@) and HTTP status (@403@) in the rendered error rather than
-- amazonka's shifting record/lens names.
checkConnection :: Env -> IO ConnCheck
checkConnection env = do
  r <- try (listAllBuckets env)
  pure $ case r of
    Right _ -> ConnOK
    Left (e :: SomeException) ->
      let msg = T.pack (displayException e)
       in if isDenied msg then ConnDenied else ConnFailed (oneLine msg)
  where
    isDenied m =
      "AccessDenied" `T.isInfixOf` m
        || "statusCode = 403" `T.isInfixOf` m
    oneLine = T.unwords . T.words

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
--
-- Uses 'AWS.hashedFile' (a single SHA256-signed payload) rather than
-- 'AWS.chunkedFile'. Chunked uploads set @x-amz-content-sha256:
-- STREAMING-AWS4-HMAC-SHA256-PAYLOAD@ with @Content-Encoding: aws-chunked@,
-- which corporate reverse proxies often reject or rewrite; a hashed body is
-- an ordinary signed request that passes through cleanly.
uploadFile :: Env -> Text -> Text -> FilePath -> IO ()
uploadFile env bucket key path = do
  body <- AWS.hashedFile path
  runResourceT . void $
    AWS.send env (S3.newPutObject (S3.BucketName bucket) (S3.ObjectKey key) (AWS.toBody body))

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
