{-# LANGUAGE OverloadedStrings #-}

-- | Reading and writing named profiles in @~\/.aws\/credentials@.
--
-- This is deliberately tiny: it understands @[section]@ headers and
-- @key = value@ lines, ignores comments (@#@ / @;@) and blank lines, and
-- nothing else. It is enough to list the profiles yeez can offer in the
-- setup wizard, load one back into 'ConnParams', and persist a new one.
--
-- Region and endpoint live in the same section as the keys so that a
-- yeez-written profile round-trips through 'loadProfile'. @region@ is a key
-- the AWS CLI reads too; @endpoint_url@ is understood by the AWS CLI v2 and
-- simply ignored by tools that do not know it.
--
-- Note: 'saveProfile' rewrites the file from its parsed @key = value@ pairs,
-- so hand-written comments and blank-line formatting in the file are not
-- preserved.
module Config
  ( listProfiles
  , loadProfile
  , saveProfile
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import S3.Client (ConnParams (..))
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getHomeDirectory
  )
import System.FilePath (takeDirectory, (</>))

-- | Absolute path of @~\/.aws\/credentials@.
credentialsPath :: IO FilePath
credentialsPath = do
  home <- getHomeDirectory
  pure (home </> ".aws" </> "credentials")

-- | Names of all profiles (section headers) in @~\/.aws\/credentials@,
-- in file order. Empty if the file is absent.
listProfiles :: IO [Text]
listProfiles = map fst <$> readIni

-- | Load a profile into 'ConnParams'. Returns 'Nothing' if the profile is
-- absent or lacks a static access-key / secret-key pair (e.g. an SSO-only
-- profile, which yeez cannot use directly).
loadProfile :: Text -> IO (Maybe ConnParams)
loadProfile name = do
  ini <- readIni
  pure $ do
    kvs <- lookup name ini
    access <- lookup "aws_access_key_id" kvs
    secret <- lookup "aws_secret_access_key" kvs
    let region = maybe "us-east-1" id (lookup "region" kvs)
        endpoint = lookup "endpoint_url" kvs
    pure
      ConnParams
        { cpAccessKey = access
        , cpSecretKey = secret
        , cpRegion = region
        , cpEndpoint = endpoint
        }

-- | Write (or replace) a named profile in @~\/.aws\/credentials@, creating
-- the file and its @~\/.aws@ directory if necessary. Other profiles are
-- preserved.
saveProfile :: Text -> ConnParams -> IO ()
saveProfile name cp = do
  path <- credentialsPath
  createDirectoryIfMissing True (takeDirectory path)
  ini <- readIni
  let without = filter ((/= name) . fst) ini
      updated = without ++ [(name, sectionKVs cp)]
  TIO.writeFile path (renderIni updated)

-- | The key/value lines for a profile.
sectionKVs :: ConnParams -> [(Text, Text)]
sectionKVs cp =
  [ ("aws_access_key_id", cpAccessKey cp)
  , ("aws_secret_access_key", cpSecretKey cp)
  , ("region", cpRegion cp)
  ]
    ++ maybe [] (\e -> [("endpoint_url", e)]) (cpEndpoint cp)

-- ---------------------------------------------------------------------------
-- Minimal INI
-- ---------------------------------------------------------------------------

-- | Parse @~\/.aws\/credentials@ into ordered sections. Absent file → @[]@.
readIni :: IO [(Text, [(Text, Text)])]
readIni = do
  path <- credentialsPath
  exists <- doesFileExist path
  if not exists
    then pure []
    else parseIni <$> TIO.readFile path

parseIni :: Text -> [(Text, [(Text, Text)])]
parseIni = go Nothing [] . map T.strip . T.lines
  where
    go cur acc [] = flush cur acc
    go cur acc (l : ls)
      | T.null l || isComment l = go cur acc ls
      | Just header <- section l = go (Just (header, [])) (flush cur acc) ls
      | Just kv <- keyVal l = case cur of
          Just (h, kvs) -> go (Just (h, kvs ++ [kv])) acc ls
          Nothing -> go cur acc ls
      | otherwise = go cur acc ls

    -- Append the in-progress section (if any) to the accumulator.
    flush Nothing acc = acc
    flush (Just s) acc = acc ++ [s]

    isComment l = "#" `T.isPrefixOf` l || ";" `T.isPrefixOf` l

    section l = do
      inner <- T.stripPrefix "[" l >>= (\t -> T.stripSuffix "]" (T.strip t))
      pure (T.strip inner)

    keyVal l = case T.breakOn "=" l of
      (k, v)
        | not (T.null v) -> Just (T.strip k, T.strip (T.drop 1 v))
      _ -> Nothing

renderIni :: [(Text, [(Text, Text)])] -> Text
renderIni = T.intercalate "\n" . concatMap renderSection
  where
    renderSection (name, kvs) =
      ("[" <> name <> "]") : map renderKV kvs ++ [""]
    renderKV (k, v) = k <> " = " <> v
