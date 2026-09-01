{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Drawing and event handling: the whole yeez state machine.
module UI.App (runApp) where

import Amazonka (Env)
import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Control.Exception (SomeException, displayException, try)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Zipper as Z
import qualified Data.Vector as V
import qualified Graphics.Vty as Vty
import S3.Client
import System.Directory
  ( doesDirectoryExist
  , getHomeDirectory
  )
import System.Exit (exitFailure)
import System.FilePath ((</>), takeFileName)
import System.IO (hPutStrLn, stderr)
import UI.Types

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

-- | Build the AWS environment, fetch the bucket list and run the TUI.
runApp :: IO ()
runApp = do
  envOrErr <- try newAwsEnv
  case envOrErr of
    Left (e :: SomeException) -> do
      hPutStrLn stderr ("yeez: could not initialise AWS credentials:\n" <> displayException e)
      exitFailure
    Right env -> do
      bucketsOrErr <- try (listAllBuckets env)
      let st0 = initialState env
          st = case bucketsOrErr of
            Left (e :: SomeException) ->
              st0
                { stStatus = "error listing buckets: " <> oneLine e
                , stScreen = ScreenMessage ScreenBuckets
                }
            Right bs ->
              st0
                { stBuckets = L.listReplace (V.fromList bs) (initialSel bs) (stBuckets st0)
                , stStatus = countLabel (length bs) "bucket"
                }
      void (defaultMain app st)

initialState :: Env -> AppState
initialState env =
  AppState
    { stEnv = env
    , stBuckets = L.list BucketListW V.empty 1
    , stObjects = L.list ObjectListW V.empty 1
    , stBucket = Nothing
    , stPrefix = ""
    , stScreen = ScreenBuckets
    , stEditor = emptyEditor
    , stStatus = ""
    , stPendingDelete = Nothing
    }

app :: App AppState e Name
app =
  App
    { appDraw = drawUI
    , appChooseCursor = chooseCursor
    , appHandleEvent = appEvent
    , appStartEvent = pure ()
    , appAttrMap = const theMap
    }

chooseCursor :: AppState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor st = case stScreen st of
  ScreenPrompt _ -> showCursorNamed PathEditorW
  _ -> neverShowCursor st

-- ---------------------------------------------------------------------------
-- Drawing
-- ---------------------------------------------------------------------------

drawUI :: AppState -> [Widget Name]
drawUI st = case stScreen st of
  ScreenBuckets -> [bucketsScreen st]
  ScreenObjects -> [objectsScreen st]
  ScreenPrompt act -> [promptOverlay st act, backdrop st ScreenObjects]
  ScreenConfirmDelete -> [confirmOverlay st, backdrop st ScreenObjects]
  ScreenMessage back -> [messageOverlay st, backdrop st back]

-- | The full-screen layer drawn underneath an overlay.
backdrop :: AppState -> Screen -> Widget Name
backdrop st = \case
  ScreenBuckets -> bucketsScreen st
  _ -> if stBucket st == Nothing then bucketsScreen st else objectsScreen st

header :: Text -> Widget Name
header t = withAttr titleAttr (padRight Max (txt (" " <> t)))

footer :: AppState -> Text -> Widget Name
footer st keys =
  vBox
    [ withAttr statusAttr (padRight Max (txt (" " <> stStatus st)))
    , withAttr helpAttr (padRight Max (txt (" " <> keys)))
    ]

bucketsScreen :: AppState -> Widget Name
bucketsScreen st =
  vBox
    [ header "yeez — buckets"
    , B.hBorder
    , L.renderList renderBucket True (stBuckets st)
    , B.hBorder
    , footer st "↑/↓ move · enter open · r refresh · q quit"
    ]

renderBucket :: Bool -> Text -> Widget Name
renderBucket _ b = padRight Max (txt ("  " <> b))

objectsScreen :: AppState -> Widget Name
objectsScreen st =
  vBox
    [ header ("yeez — s3://" <> fromMaybe "" (stBucket st) <> "/" <> stPrefix st)
    , B.hBorder
    , L.renderList renderObject True (stObjects st)
    , B.hBorder
    , footer st "enter open · esc/h up · u upload · d download · n new folder · R rename · x delete · r refresh · q quit"
    ]

renderObject :: Bool -> ObjectRow -> Widget Name
renderObject _ row =
  padRight Max $
    hBox
      [ txt "  "
      , if rowIsFolder row
          then withAttr folderAttr (txt (rowName row <> "/"))
          else txt (rowName row)
      , padLeft Max (txt (sizeLabel row <> " "))
      ]

sizeLabel :: ObjectRow -> Text
sizeLabel row
  | rowIsFolder row = "<dir>"
  | otherwise = maybe "" humanBytes (rowSize row)

promptOverlay :: AppState -> PendingAction -> Widget Name
promptOverlay st act =
  overlay (promptLabel act) $
    vBox
      [ hLimit 60 (vLimit 1 (E.renderEditor (txt . T.concat) True (stEditor st)))
      , withAttr helpAttr (txt "enter submit · esc cancel")
      ]

confirmOverlay :: AppState -> Widget Name
confirmOverlay st =
  overlay "Confirm delete" $
    vBox
      [ txt ("Delete " <> maybe "?" rowKey (stPendingDelete st) <> " ?")
      , withAttr helpAttr (txt "y delete · n/esc cancel")
      ]

messageOverlay :: AppState -> Widget Name
messageOverlay st =
  overlay "yeez" $
    vBox
      [ txtWrap (stStatus st)
      , withAttr helpAttr (txt "press any key to continue")
      ]

overlay :: Text -> Widget Name -> Widget Name
overlay title body =
  C.centerLayer . B.borderWithLabel (txt (" " <> title <> " ")) . hLimit 70 . padAll 1 $ body

-- ---------------------------------------------------------------------------
-- Events
-- ---------------------------------------------------------------------------

appEvent :: BrickEvent Name e -> EventM Name AppState ()
appEvent be = do
  scr <- gets stScreen
  case scr of
    ScreenPrompt act -> promptEvent act be
    _ -> case be of
      VtyEvent ev -> case scr of
        ScreenBuckets -> bucketsEvent ev
        ScreenObjects -> objectsEvent ev
        ScreenConfirmDelete -> confirmEvent ev
        ScreenMessage back -> modify (\s -> s { stScreen = back })
      _ -> pure ()

bucketsEvent :: Vty.Event -> EventM Name AppState ()
bucketsEvent = \case
  Vty.EvKey (Vty.KChar 'q') [] -> halt
  Vty.EvKey Vty.KEsc [] -> halt
  Vty.EvKey (Vty.KChar 'r') [] -> refreshBuckets
  Vty.EvKey Vty.KEnter [] -> openSelectedBucket
  ev -> zoom bucketsL (L.handleListEvent ev)

objectsEvent :: Vty.Event -> EventM Name AppState ()
objectsEvent = \case
  Vty.EvKey (Vty.KChar 'q') [] -> halt
  Vty.EvKey Vty.KEsc [] -> goUp
  Vty.EvKey (Vty.KChar 'h') [] -> goUp
  Vty.EvKey Vty.KEnter [] -> openSelectedRow
  Vty.EvKey (Vty.KChar 'r') [] -> refreshObjects
  Vty.EvKey (Vty.KChar 'u') [] -> startPrompt ActUpload ""
  Vty.EvKey (Vty.KChar 'n') [] -> startPrompt ActNewFolder ""
  Vty.EvKey (Vty.KChar 'd') [] -> withSelectedFile "download" $ \row ->
    startPrompt ActDownload (rowName row)
  Vty.EvKey (Vty.KChar 'R') [] -> withSelectedFile "rename" $ \row ->
    startPrompt ActRename (rowName row)
  Vty.EvKey (Vty.KChar 'x') [] -> withSelectedFile "delete" $ \row ->
    modify (\s -> s { stPendingDelete = Just row, stScreen = ScreenConfirmDelete })
  ev -> zoom objectsL (L.handleListEvent ev)

confirmEvent :: Vty.Event -> EventM Name AppState ()
confirmEvent = \case
  Vty.EvKey (Vty.KChar 'y') [] -> do
    st <- get
    case (stBucket st, stPendingDelete st) of
      (Just b, Just row) -> do
        modify (\s -> s { stPendingDelete = Nothing })
        runS3 (deleteKey (stEnv st) b (rowKey row)) $ \() -> do
          refreshObjects
          message ("deleted " <> rowKey row)
      _ -> cancelToObjects
  Vty.EvKey (Vty.KChar 'n') [] -> cancelToObjects
  Vty.EvKey Vty.KEsc [] -> cancelToObjects
  _ -> pure ()

cancelToObjects :: EventM Name AppState ()
cancelToObjects =
  modify (\s -> s { stScreen = ScreenObjects, stPendingDelete = Nothing })

promptEvent :: PendingAction -> BrickEvent Name e -> EventM Name AppState ()
promptEvent act be = case be of
  VtyEvent (Vty.EvKey Vty.KEsc []) -> modify (\s -> s { stScreen = ScreenObjects })
  VtyEvent (Vty.EvKey Vty.KEnter []) -> submitPrompt act
  _ -> zoom editorL (E.handleEditorEvent be)

-- | Open the prompt with @initial@ pre-filled and the cursor at its end.
startPrompt :: PendingAction -> Text -> EventM Name AppState ()
startPrompt act initial =
  modify $ \s ->
    s
      { stScreen = ScreenPrompt act
      , stEditor = E.applyEdit Z.gotoEOF (E.editor PathEditorW (Just 1) initial)
      }

submitPrompt :: PendingAction -> EventM Name AppState ()
submitPrompt act = do
  st <- get
  let input = T.strip (T.concat (E.getEditContents (stEditor st)))
  modify (\s -> s { stScreen = ScreenObjects })
  case stBucket st of
    Nothing -> message "no bucket open"
    Just bucket
      | T.null input -> message "cancelled: empty input"
      | otherwise -> case act of
          ActUpload -> do
            path <- liftIO (expandUser (T.unpack input))
            let key = stPrefix st <> T.pack (takeFileName path)
            runS3 (uploadFile (stEnv st) bucket key path) $ \() -> do
              refreshObjects
              message ("uploaded " <> T.pack path <> " → " <> key)
          ActDownload -> case L.listSelectedElement (stObjects st) of
            Just (_, row) | not (rowIsFolder row) -> do
              dest <- liftIO (resolveDest (T.unpack input) (rowName row))
              runS3 (downloadFile (stEnv st) bucket (rowKey row) dest) $ \() ->
                message ("downloaded " <> rowKey row <> " → " <> T.pack dest)
            _ -> message "select a file to download"
          ActNewFolder -> do
            let key = stPrefix st <> stripSlashes input <> "/"
            runS3 (createFolderMarker (stEnv st) bucket key) $ \() -> do
              refreshObjects
              message ("created folder " <> key)
          ActRename -> case L.listSelectedElement (stObjects st) of
            Just (_, row) | not (rowIsFolder row) -> do
              let dst = stPrefix st <> stripSlashes input
              if dst == rowKey row
                then message "cancelled: same key"
                else runS3 (copyKey (stEnv st) bucket (rowKey row) dst) $ \() ->
                  runS3 (deleteKey (stEnv st) bucket (rowKey row)) $ \() -> do
                    refreshObjects
                    message ("renamed " <> rowKey row <> " → " <> dst)
            _ -> message "select a file to rename"

-- ---------------------------------------------------------------------------
-- Navigation and loading
-- ---------------------------------------------------------------------------

openSelectedBucket :: EventM Name AppState ()
openSelectedBucket = do
  st <- get
  case L.listSelectedElement (stBuckets st) of
    Nothing -> pure ()
    Just (_, b) -> do
      modify (\s -> s { stBucket = Just b, stPrefix = "", stScreen = ScreenObjects })
      refreshObjects

openSelectedRow :: EventM Name AppState ()
openSelectedRow = do
  st <- get
  case L.listSelectedElement (stObjects st) of
    Just (_, row) | rowIsFolder row -> do
      modify (\s -> s { stPrefix = rowKey row })
      refreshObjects
    Just (_, row) -> message (rowKey row <> maybe "" (\n -> " · " <> humanBytes n) (rowSize row))
    Nothing -> pure ()

goUp :: EventM Name AppState ()
goUp = do
  st <- get
  if T.null (stPrefix st)
    then modify $ \s ->
      s
        { stScreen = ScreenBuckets
        , stBucket = Nothing
        , stStatus = countLabel (length (stBuckets s)) "bucket"
        }
    else do
      modify (\s -> s { stPrefix = parentPrefix (stPrefix s) })
      refreshObjects

refreshBuckets :: EventM Name AppState ()
refreshBuckets = do
  st <- get
  runS3 (listAllBuckets (stEnv st)) $ \bs ->
    modify $ \s ->
      s
        { stBuckets = L.listReplace (V.fromList bs) (initialSel bs) (stBuckets s)
        , stStatus = countLabel (length bs) "bucket"
        }

refreshObjects :: EventM Name AppState ()
refreshObjects = do
  st <- get
  case stBucket st of
    Nothing -> pure ()
    Just b -> runS3 (listObjectsUnder (stEnv st) b (stPrefix st)) $ \(dirs, files) -> do
      let prefix = stPrefix st
          folderRows =
            [ ObjectRow p (segmentAfter prefix p) True Nothing
            | p <- dirs
            ]
          fileRows =
            [ ObjectRow k (segmentAfter prefix k) False (Just sz)
            | (k, sz) <- files
              -- the zero-byte marker for the folder we are looking at
            , k /= prefix
            ]
          rows = sortOn rowName folderRows <> sortOn rowName fileRows
      modify $ \s ->
        s
          { stObjects = L.listReplace (V.fromList rows) (initialSel rows) (stObjects s)
          , stStatus =
              countLabel (length folderRows) "folder"
                <> ", "
                <> countLabel (length fileRows) "object"
          }

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Run a blocking S3 call, turning any exception into a message screen.
runS3 :: IO a -> (a -> EventM Name AppState ()) -> EventM Name AppState ()
runS3 act k = do
  r <- liftIO (try act)
  case r of
    Left (e :: SomeException) -> message ("error: " <> oneLine e)
    Right a -> k a

-- | Show a transient message, returning afterwards to the current listing.
message :: Text -> EventM Name AppState ()
message t = modify $ \s ->
  s
    { stStatus = t
    , stScreen = ScreenMessage (if stBucket s == Nothing then ScreenBuckets else ScreenObjects)
    }

withSelectedFile :: Text -> (ObjectRow -> EventM Name AppState ()) -> EventM Name AppState ()
withSelectedFile what k = do
  st <- get
  case L.listSelectedElement (stObjects st) of
    Just (_, row) | not (rowIsFolder row) -> k row
    Just _ -> message ("cannot " <> what <> " a folder")
    Nothing -> message ("nothing to " <> what)

oneLine :: SomeException -> Text
oneLine = T.unwords . T.words . T.pack . displayException

initialSel :: [a] -> Maybe Int
initialSel xs = if null xs then Nothing else Just 0

countLabel :: Int -> Text -> Text
countLabel n what = T.pack (show n) <> " " <> what <> (if n == 1 then "" else "s")

-- | The part of @key@ directly below @prefix@, without a trailing slash.
segmentAfter :: Text -> Text -> Text
segmentAfter prefix key = stripSlashes (fromMaybe key (T.stripPrefix prefix key))

stripSlashes :: Text -> Text
stripSlashes = T.dropWhile (== '/') . T.dropWhileEnd (== '/')

-- | @\"a\/b\/c\/\" -> \"a\/b\/\"@, @\"a\/\" -> \"\"@.
parentPrefix :: Text -> Text
parentPrefix p =
  let trimmed = T.dropWhileEnd (== '/') p
   in case T.breakOnEnd "/" trimmed of
        (parent, _) -> parent

humanBytes :: Integer -> Text
humanBytes n = go (fromIntegral n :: Double) units
  where
    units = ["B", "K", "M", "G", "T", "P"] :: [Text]
    go x [u] = fmt x u
    go x (u : us)
      | x < 1024 = fmt x u
      | otherwise = go (x / 1024) us
    go _ [] = ""
    fmt x u
      | u == "B" = T.pack (show (round x :: Integer)) <> u
      | x < 10 = T.pack (showFixed1 x) <> u
      | otherwise = T.pack (show (round x :: Integer)) <> u
    showFixed1 x =
      let r = fromIntegral (round (x * 10) :: Integer) / 10 :: Double
          (i, f) = properFraction r :: (Integer, Double)
       in show i <> "." <> show (round (f * 10) :: Integer)

-- | Expand a leading @~/@ to the user's home directory.
expandUser :: FilePath -> IO FilePath
expandUser p = case p of
  '~' : '/' : rest -> do
    home <- getHomeDirectory
    pure (home </> rest)
  _ -> pure p

-- | If the destination is an existing directory, download into it under
-- the object's own name.
resolveDest :: FilePath -> Text -> IO FilePath
resolveDest rawPath name = do
  p <- expandUser rawPath
  isDir <- doesDirectoryExist p
  pure (if isDir then p </> T.unpack name else p)

emptyEditor :: E.Editor Text Name
emptyEditor = E.editor PathEditorW (Just 1) ""

-- ---------------------------------------------------------------------------
-- Attributes
-- ---------------------------------------------------------------------------

titleAttr, statusAttr, helpAttr, folderAttr :: A.AttrName
titleAttr = A.attrName "title"
statusAttr = A.attrName "status"
helpAttr = A.attrName "help"
folderAttr = A.attrName "folder"

theMap :: A.AttrMap
theMap =
  A.attrMap
    Vty.defAttr
    [ (L.listAttr, Vty.defAttr)
    , (L.listSelectedAttr, Vty.black `on` Vty.cyan)
    , (E.editAttr, Vty.defAttr)
    , (E.editFocusedAttr, Vty.black `on` Vty.white)
    , (titleAttr, Vty.white `on` Vty.blue)
    , (statusAttr, fg Vty.yellow)
    , (helpAttr, fg Vty.brightBlack)
    , (folderAttr, fg Vty.brightBlue)
    ]
