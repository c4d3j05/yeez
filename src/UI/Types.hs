{-# LANGUAGE OverloadedStrings #-}

-- | Shared state and types for the yeez terminal UI.
--
-- This module is deliberately free of any drawing or event-handling code:
-- "UI.App" owns those, and only needs the vocabulary defined here.
module UI.Types
  ( -- * Widget names
    Name (..)

    -- * Screens
  , Screen (..)
  , PendingAction (..)
  , promptLabel

    -- * Object rows
  , ObjectRow (..)

    -- * Connections
  , Conn (..)
  , curEnv

    -- * Application state
  , AppState (..)
  , bucketsL
  , objectsL
  , editorL
  , connListL
  ) where

import Amazonka (Env)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Data.Text (Text)
import Lens.Micro (Lens')

-- | Brick widget identifiers. Every focusable widget needs a unique name.
data Name
  = BucketListW
  | ObjectListW
  | PathEditorW
  | ConnListW
  deriving (Eq, Ord, Show)

-- | What the single-line prompt is currently collecting input for.
data PendingAction
  = ActUpload
  | ActDownload
  | ActNewFolder
  | ActRename
  deriving (Eq, Show)

-- | The five screens of the app. The whole UI is a state machine over these.
data Screen
  = ScreenBuckets
    -- ^ Top level: the list of buckets.
  | ScreenObjects
    -- ^ Objects and folders under the current bucket + prefix.
  | ScreenPrompt PendingAction
    -- ^ Single-line text input; the payload says what the input is for.
  | ScreenConfirmDelete
    -- ^ y\/n gate in front of a delete.
  | ScreenMessage Screen
    -- ^ Transient status message; the payload is the screen to return to.
  | ScreenConnections
    -- ^ The connection switcher: pick an open connection or add a new one.
  | ScreenHelp Screen
    -- ^ A full command reference; the payload is the screen to return to.
  deriving (Eq, Show)

-- | Human-readable prompt text for each thing the prompt can collect.
promptLabel :: PendingAction -> Text
promptLabel a = case a of
  ActUpload    -> "Upload — local file path"
  ActDownload  -> "Download — local destination path"
  ActNewFolder -> "New folder name"
  ActRename    -> "Rename to"

-- | One row of the object list.
--
-- Folders are synthetic: S3 has no directories, so a row with
-- 'rowIsFolder' set comes from a @ListObjectsV2@ common prefix rather
-- than from a real object.
data ObjectRow = ObjectRow
  { rowKey :: Text
    -- ^ Full S3 key (for folders, the common prefix, with trailing @\/@).
  , rowName :: Text
    -- ^ Display name: the last path segment.
  , rowIsFolder :: Bool
    -- ^ Whether this row is a synthetic folder.
  , rowSize :: Maybe Integer
    -- ^ Size in bytes, for files only.
  } deriving (Eq, Show)

-- | One open connection: a validated AWS 'Env' plus a human-readable label
-- (a profile name, a saved-as name, an endpoint host, or @\"detected\"@).
data Conn = Conn
  { connLabel :: Text
    -- ^ Display name shown in the connection switcher.
  , connEnv :: Env
    -- ^ The validated AWS environment for this connection.
  }

-- | The AWS environment of the currently active connection.
curEnv :: AppState -> Env
curEnv st = connEnv (stConns st !! stConnIx st)

-- | The entire application state.
data AppState = AppState
  { stConns :: [Conn]
    -- ^ All open connections; yeez can switch between them live.
  , stConnIx :: Int
    -- ^ Index into 'stConns' of the active connection.
  , stConnList :: L.List Name (Maybe Int)
    -- ^ Switcher rows: @Just i@ selects connection @i@, 'Nothing' is the
    -- \"add a new connection\" row. Rebuilt each time the switcher opens.
  , stBuckets :: L.List Name Text
    -- ^ Bucket names.
  , stObjects :: L.List Name ObjectRow
    -- ^ Objects and folders under 'stBucket' + 'stPrefix'.
  , stBucket :: Maybe Text
    -- ^ Currently open bucket, if any.
  , stPrefix :: Text
    -- ^ Current prefix inside the bucket; @\"\"@ or ends with @\/@.
  , stScreen :: Screen
    -- ^ Which screen is active.
  , stEditor :: E.Editor Text Name
    -- ^ The single-line path\/name input used by 'ScreenPrompt'.
  , stStatus :: Text
    -- ^ Status line / message text.
  , stPendingDelete :: Maybe ObjectRow
    -- ^ Row awaiting confirmation on 'ScreenConfirmDelete'.
  }

bucketsL :: Lens' AppState (L.List Name Text)
bucketsL f s = (\x -> s { stBuckets = x }) <$> f (stBuckets s)

objectsL :: Lens' AppState (L.List Name ObjectRow)
objectsL f s = (\x -> s { stObjects = x }) <$> f (stObjects s)

editorL :: Lens' AppState (E.Editor Text Name)
editorL f s = (\x -> s { stEditor = x }) <$> f (stEditor s)

connListL :: Lens' AppState (L.List Name (Maybe Int))
connListL f s = (\x -> s { stConnList = x }) <$> f (stConnList s)
