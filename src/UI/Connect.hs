{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The connection setup wizard: a small, self-contained Brick app that
-- runs before the main UI and hands back a connected 'Env'.
--
-- Keeping it separate from "UI.App" means the main application never has to
-- reason about a not-yet-connected state — by the time it runs, it already
-- has a validated 'Env'.
--
-- The wizard offers three ways to connect:
--
--   * __Detected credentials__ — whatever @Amazonka.discover@ found
--     (environment variables / default profile), passed in by the caller.
--   * __A named profile__ from @~\/.aws\/credentials@.
--   * __A new connection__ typed in by hand (keys, region and an optional
--     S3-compatible endpoint), optionally saved back as a profile.
--
-- Every choice is validated with a real @ListBuckets@ call before the
-- wizard returns, so a bad credential fails here rather than on the first
-- action inside the app.
--
-- A TLS trust failure is treated as recoverable rather than fatal: instead
-- of dropping the user at an error they cannot act on, the wizard asks for
-- the path to a PEM CA bundle and retries the same connection with it. See
-- 'Attempt', which is what makes that retry possible.
module UI.Connect (connect) where

import Amazonka (Env)
import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Config (loadProfile, saveProfile)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Zipper as Z
import qualified Data.Vector as V
import qualified Graphics.Vty as Vty
import Lens.Micro (Lens')
import S3.Client
  ( ConnCheck (..)
  , ConnParams (..)
  , checkConnection
  , isCertificateError
  , newAwsEnvFromParams
  , newAwsEnvWith
  )
import System.Environment (setEnv)
import UI.Types (expandUser)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Widget names for the wizard.
data CName = CList | CEdit
  deriving (Eq, Ord, Show)

-- | A row in the connection picker.
data CItem
  = IDiscover
    -- ^ Use credentials already discovered from the environment.
  | IProfile Text
    -- ^ Use a named @~\/.aws\/credentials@ profile.
  | INew
    -- ^ Enter a new connection by hand.

-- | The fields of the "new connection" form, collected one at a time.
data Step = SAccess | SSecret | SRegion | SEndpoint | SSaveAs
  deriving (Eq, Show)

-- | Draft of a hand-entered connection.
data Draft = Draft
  { dAccess :: Text
  , dSecret :: Text
  , dRegion :: Text
  , dEndpoint :: Text
  , dSaveAs :: Text
  }

emptyDraft :: Draft
emptyDraft = Draft "" "" "us-east-1" "" ""

-- | Which part of the wizard is on screen.
data CScreen
  = CPick
    -- ^ The connection picker list.
  | CField Step
    -- ^ Collecting one field of the new-connection form.
  | CMessage
    -- ^ A status/error message; any key returns to the picker.
  | CCaBundle
    -- ^ Asking for a CA bundle path after a certificate failure.

-- | A connection attempt, in a form that can be run more than once.
--
-- Recovering from a certificate error means building the /same/ connection
-- again with a different trust store, so an attempt cannot be a plain
-- @IO Env@ that has already baked in its TLS settings. Parameterising both
-- the env and the save step over the bundle path keeps the retry honest:
-- the connection that gets validated is the one that gets saved.
data Attempt = Attempt
  { atLabel :: Text
    -- ^ Display name for the resulting connection.
  , atEnv :: Maybe FilePath -> IO Env
    -- ^ Build the environment, optionally trusting a CA bundle.
  , atSave :: Maybe FilePath -> IO ()
    -- ^ Run on success (persisting a profile, or nothing at all).
  }

data CState = CState
  { csList :: L.List CName CItem
  , csScreen :: CScreen
  , csDraft :: Draft
  , csEditor :: E.Editor Text CName
  , csStatus :: Text
  , csDiscover :: Maybe Env
    -- ^ Pre-discovered env, offered as 'IDiscover'.
  , csPending :: Maybe Attempt
    -- ^ The attempt that hit a certificate error, kept so 'CCaBundle' can
    -- retry it once the user supplies a bundle.
  , csCaPath :: Text
    -- ^ Last CA bundle path entered, pre-filled on the next prompt. One
    -- bundle usually fixes every endpoint behind the same proxy.
  , csResult :: Maybe (Text, Env)
    -- ^ Set once a connection validates (label plus env); the app halts
    -- immediately after.
  }

csListL :: Lens' CState (L.List CName CItem)
csListL f s = (\x -> s { csList = x }) <$> f (csList s)

csEditorL :: Lens' CState (E.Editor Text CName)
csEditorL f s = (\x -> s { csEditor = x }) <$> f (csEditor s)

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

-- | Run the wizard. @mDisc@ is the environment discovered from the ambient
-- credential chain (offered as the first choice when present); @profiles@
-- are the named profiles found in @~\/.aws\/credentials@. Returns a labelled
-- connection (a display name plus the validated 'Env'), or 'Nothing' if the
-- user quit without connecting.
connect :: Maybe Env -> [Text] -> IO (Maybe (Text, Env))
connect mDisc profiles = do
  let items =
        maybe [] (const [IDiscover]) mDisc
          ++ map IProfile profiles
          ++ [INew]
      st0 =
        CState
          { csList = L.list CList (V.fromList items) 1
          , csScreen = CPick
          , csDraft = emptyDraft
          , csEditor = editorFor SAccess emptyDraft
          , csStatus = ""
          , csDiscover = mDisc
          , csPending = Nothing
          , csCaPath = ""
          , csResult = Nothing
          }
  csResult <$> defaultMain wizardApp st0

wizardApp :: App CState e CName
wizardApp =
  App
    { appDraw = cDraw
    , appChooseCursor = cChooseCursor
    , appHandleEvent = cEvent
    , appStartEvent = pure ()
    , appAttrMap = const theMap
    }

cChooseCursor :: CState -> [CursorLocation CName] -> Maybe (CursorLocation CName)
cChooseCursor st = case csScreen st of
  CField _ -> showCursorNamed CEdit
  CCaBundle -> showCursorNamed CEdit
  _ -> neverShowCursor st

-- ---------------------------------------------------------------------------
-- Drawing
-- ---------------------------------------------------------------------------

cDraw :: CState -> [Widget CName]
cDraw st = case csScreen st of
  CPick -> [pickScreen st]
  CField step -> [fieldOverlay st step, pickScreen st]
  CMessage -> [msgOverlay st, pickScreen st]
  CCaBundle -> [caOverlay st, pickScreen st]

pickScreen :: CState -> Widget CName
pickScreen st =
  vBox
    [ withAttr titleAttr (padRight Max (txt " yeez — connect"))
    , B.hBorder
    , L.renderList renderItem True (csList st)
    , B.hBorder
    , withAttr helpAttr (padRight Max (txt " ↑/↓ move · enter select · q quit"))
    ]

renderItem :: Bool -> CItem -> Widget CName
renderItem _ it = padRight Max (txt label)
  where
    label = case it of
      IDiscover -> "  Detected credentials (environment / default profile)"
      IProfile n -> "  profile: " <> n
      INew -> "  + New connection…"

fieldOverlay :: CState -> Step -> Widget CName
fieldOverlay st step =
  overlay (stepLabel step) $
    vBox
      [ hLimit 60 (vLimit 1 (E.renderEditor (renderContents step) True (csEditor st)))
      , withAttr helpAttr (txt "enter next · esc back")
      ]
  where
    renderContents SSecret ts = txt (T.replicate (T.length (T.concat ts)) "•")
    renderContents _ ts = txt (T.concat ts)

-- | The certificate-error recovery prompt: what went wrong, plus a path
-- box for the CA bundle to trust instead of the system store.
caOverlay :: CState -> Widget CName
caOverlay st =
  overlay "Certificate problem" $
    vBox
      [ txtWrap (briefly (csStatus st))
      , txt " "
      , txt "Path to a CA bundle in PEM format to trust for this connection:"
      , hLimit 60 (vLimit 1 (E.renderEditor (txt . T.concat) True (csEditor st)))
      , withAttr helpAttr (txt "enter retry · esc cancel")
      ]

-- | Trim a rendered exception down for the certificate prompt, keeping the
-- /end/ of it.
--
-- amazonka renders the entire 'Request' record into its errors, so the head
-- of the message is nothing but headers and hostnames while the part that
-- says @certificate has unknown CA@ sits at the very end. Keeping the head
-- would show the user only noise and push the input box off a short
-- terminal; the full text is still shown on the message screen.
briefly :: Text -> Text
briefly t
  | T.length t <= limit = t
  | otherwise = "… " <> T.takeEnd limit t
  where
    limit = 240

msgOverlay :: CState -> Widget CName
msgOverlay st =
  overlay "yeez" $
    vBox
      [ txtWrap (csStatus st)
      , withAttr helpAttr (txt "press any key to continue")
      ]

overlay :: Text -> Widget CName -> Widget CName
overlay title body =
  C.centerLayer . B.borderWithLabel (txt (" " <> title <> " ")) . hLimit 70 . padAll 1 $ body

-- ---------------------------------------------------------------------------
-- Events
-- ---------------------------------------------------------------------------

cEvent :: BrickEvent CName e -> EventM CName CState ()
cEvent be = do
  scr <- gets csScreen
  case scr of
    CPick -> case be of
      VtyEvent ev -> pickEvent ev
      _ -> pure ()
    CField step -> case be of
      VtyEvent (Vty.EvKey Vty.KEsc []) -> toPick
      VtyEvent (Vty.EvKey Vty.KEnter []) -> storeAndAdvance step
      _ -> zoom csEditorL (E.handleEditorEvent be)
    CMessage -> case be of
      VtyEvent _ -> toPick
      _ -> pure ()
    CCaBundle -> case be of
      VtyEvent (Vty.EvKey Vty.KEsc []) -> showPendingError
      VtyEvent (Vty.EvKey Vty.KEnter []) -> submitCaBundle
      _ -> zoom csEditorL (E.handleEditorEvent be)

toPick :: EventM CName CState ()
toPick = modify (\s -> s { csScreen = CPick })

pickEvent :: Vty.Event -> EventM CName CState ()
pickEvent = \case
  Vty.EvKey (Vty.KChar 'q') [] -> halt
  Vty.EvKey Vty.KEsc [] -> halt
  Vty.EvKey Vty.KEnter [] -> activateSelected
  ev -> zoom csListL (L.handleListEvent ev)

activateSelected :: EventM CName CState ()
activateSelected = do
  st <- get
  case L.listSelectedElement (csList st) of
    Nothing -> pure ()
    Just (_, it) -> case it of
      IDiscover -> case csDiscover st of
        Nothing -> setMsg "no detected credentials"
        Just env ->
          runAttempt
            Attempt
              { atLabel = "detected"
              , atEnv = \case
                  -- Without a bundle, reuse the env the caller discovered;
                  -- with one, rediscover so the probe uses the new store.
                  Nothing -> pure env
                  Just ca -> newAwsEnvWith (Just ca)
              , atSave = const (pure ())
              }
            Nothing
      IProfile name -> do
        mp <- liftIO (loadProfile name)
        runAttempt
          Attempt
            { atLabel = name
            , atEnv = case mp of
                Just params -> \ca -> newAwsEnvFromParams params { cpCaBundle = ca }
                -- Profile has no static keys (e.g. SSO): fall back to the
                -- discovery chain with AWS_PROFILE pointed at it.
                Nothing -> \ca ->
                  setEnv "AWS_PROFILE" (T.unpack name) >> newAwsEnvWith ca
            , atSave = const (pure ())
            }
          -- A profile that already records a ca_bundle uses it first time.
          (mp >>= cpCaBundle)
      INew -> startNew

startNew :: EventM CName CState ()
startNew =
  modify $ \s ->
    s
      { csScreen = CField SAccess
      , csDraft = emptyDraft
      , csEditor = editorFor SAccess emptyDraft
      }

-- | Store the current field into the draft and move to the next one; after
-- the last field, build the connection and try it.
storeAndAdvance :: Step -> EventM CName CState ()
storeAndAdvance step = do
  st <- get
  let v = T.strip (T.concat (E.getEditContents (csEditor st)))
      d' = stepSet step v (csDraft st)
  case nextStep step of
    Just n ->
      modify $ \s ->
        s { csDraft = d', csScreen = CField n, csEditor = editorFor n d' }
    Nothing -> do
      modify (\s -> s { csDraft = d' })
      let params = draftToParams d'
      runAttempt
        Attempt
          { atLabel = newLabel d'
          , atEnv = \ca -> newAwsEnvFromParams params { cpCaBundle = ca }
            -- Save whatever bundle actually made the connection work, so
            -- the profile reconnects cleanly without prompting again.
          , atSave = \ca ->
              unless (T.null (dSaveAs d')) $
                saveProfile (dSaveAs d') params { cpCaBundle = ca }
          }
        Nothing

-- | A display label for a hand-entered connection: the saved-as name if the
-- user chose to save it, otherwise the endpoint host or, failing that, the
-- region.
newLabel :: Draft -> Text
newLabel d
  | not (T.null (dSaveAs d)) = dSaveAs d
  | not (T.null (dEndpoint d)) = endpointHost (dEndpoint d)
  | otherwise = "custom (" <> dRegion d <> ")"
  where
    endpointHost = T.takeWhile (/= '/') . stripScheme
    stripScheme u = case T.breakOn "://" u of
      (_, r) | not (T.null r) -> T.drop 3 r
      _ -> u

-- | Run an attempt with the given CA bundle, validate it with a
-- @ListBuckets@ probe, and on success run its save step, record the
-- labelled env and halt.
--
-- A 403 / @AccessDenied@ on the probe still counts as connected: the
-- credentials are valid, they just cannot enumerate buckets, and the main UI
-- lets the user open a bucket by name.
--
-- A certificate failure is not a dead end. It can surface either while
-- building the env (an unreadable or empty bundle) or from the probe itself
-- (a server whose chain we do not trust); both route to 'CCaBundle' so the
-- user can name a trust store and retry. Only a genuine failure — bad keys,
-- unreachable endpoint — becomes a terminal message.
runAttempt :: Attempt -> Maybe FilePath -> EventM CName CState ()
runAttempt at mca = do
  r <- liftIO (try (atEnv at mca))
  case r of
    Left (e :: SomeException) ->
      let msg = oneLine e
       in if isCertificateError msg then offerCaBundle at msg else failed msg
    Right env -> do
      chk <- liftIO (checkConnection env)
      case chk of
        ConnCertError msg -> offerCaBundle at msg
        ConnFailed msg -> failed msg
        _ -> do
          liftIO (atSave at mca)
          modify (\s -> s { csResult = Just (atLabel at, env) })
          halt
  where
    failed msg = setMsg ("connection failed: " <> msg)

-- | Park a certificate-failed attempt and ask for a CA bundle path.
offerCaBundle :: Attempt -> Text -> EventM CName CState ()
offerCaBundle at msg =
  modify $ \s ->
    s
      { csScreen = CCaBundle
      , csPending = Just at
      , csStatus = msg
      , csEditor = caEditor (csCaPath s)
      }

-- | Retry the parked attempt with the bundle the user just typed. An empty
-- path means \"give up\", which falls back to showing the original error.
submitCaBundle :: EventM CName CState ()
submitCaBundle = do
  st <- get
  let typed = T.strip (T.concat (E.getEditContents (csEditor st)))
  case csPending st of
    Nothing -> toPick
    Just at
      | T.null typed -> showPendingError
      | otherwise -> do
          path <- liftIO (expandUser (T.unpack typed))
          modify (\s -> s { csCaPath = typed })
          runAttempt at (Just path)

-- | Leave the CA prompt, showing the certificate error that opened it.
showPendingError :: EventM CName CState ()
showPendingError =
  modify $ \s -> s { csScreen = CMessage, csPending = Nothing }

setMsg :: Text -> EventM CName CState ()
setMsg t = modify (\s -> s { csScreen = CMessage, csStatus = t })

-- ---------------------------------------------------------------------------
-- Form plumbing
-- ---------------------------------------------------------------------------

stepLabel :: Step -> Text
stepLabel = \case
  SAccess -> "AWS Access Key ID"
  SSecret -> "AWS Secret Access Key"
  SRegion -> "Region (e.g. us-east-1)"
  SEndpoint -> "Endpoint URL — blank for AWS (e.g. https://minio.example.com:9000)"
  SSaveAs -> "Save as profile name — blank to skip saving"

stepGet :: Step -> Draft -> Text
stepGet step = case step of
  SAccess -> dAccess
  SSecret -> dSecret
  SRegion -> dRegion
  SEndpoint -> dEndpoint
  SSaveAs -> dSaveAs

stepSet :: Step -> Text -> Draft -> Draft
stepSet step v d = case step of
  SAccess -> d { dAccess = v }
  SSecret -> d { dSecret = v }
  SRegion -> d { dRegion = v }
  SEndpoint -> d { dEndpoint = v }
  SSaveAs -> d { dSaveAs = v }

nextStep :: Step -> Maybe Step
nextStep = \case
  SAccess -> Just SSecret
  SSecret -> Just SRegion
  SRegion -> Just SEndpoint
  SEndpoint -> Just SSaveAs
  SSaveAs -> Nothing

editorFor :: Step -> Draft -> E.Editor Text CName
editorFor step d =
  E.applyEdit Z.gotoEOF (E.editor CEdit (Just 1) (stepGet step d))

-- | The CA bundle path box, pre-filled with the last path entered.
caEditor :: Text -> E.Editor Text CName
caEditor initial =
  E.applyEdit Z.gotoEOF (E.editor CEdit (Just 1) initial)

draftToParams :: Draft -> ConnParams
draftToParams d =
  ConnParams
    { cpAccessKey = dAccess d
    , cpSecretKey = dSecret d
    , cpRegion = if T.null (dRegion d) then "us-east-1" else dRegion d
    , cpEndpoint = if T.null (dEndpoint d) then Nothing else Just (dEndpoint d)
    , -- Filled in only if a certificate failure sends us to 'CCaBundle'.
      cpCaBundle = Nothing
    }

oneLine :: SomeException -> Text
oneLine = T.unwords . T.words . T.pack . displayException

-- ---------------------------------------------------------------------------
-- Attributes
-- ---------------------------------------------------------------------------

titleAttr, helpAttr :: A.AttrName
titleAttr = A.attrName "title"
helpAttr = A.attrName "help"

theMap :: A.AttrMap
theMap =
  A.attrMap
    Vty.defAttr
    [ (L.listAttr, Vty.defAttr)
    , (L.listSelectedAttr, Vty.black `on` Vty.cyan)
    , (E.editAttr, Vty.defAttr)
    , (E.editFocusedAttr, Vty.black `on` Vty.white)
    , (titleAttr, Vty.white `on` Vty.blue)
    , (helpAttr, fg Vty.brightBlack)
    ]
