{-# LANGUAGE
    ExistentialQuantification,
    Rank2Types
  #-}

module Graphics.QML.Engine (
  InitialWindowState(
    ShowWindow,
    ShowWindowWithTitle,
    HideWindow),
  EngineConfig(
    EngineConfig,
    initialURL,
    initialWindowState,
    contextObject),
  defaultEngineConfig,
  createEngine,
  runEngines
) where

import Graphics.QML.Internal.Marshal
import Graphics.QML.Internal.Objects
import Graphics.QML.Internal.Engine
import Graphics.QML.Marshal
import Graphics.QML.Objects

import Data.Maybe
import Data.Typeable
import Foreign.Storable
import Network.URI (URI, nullURI, uriPath, uriToString)

data InitialWindowState
  -- | A visible window should be created for the initial document with a
  -- default title.
  = ShowWindow
  -- | A visible window should be created for the initial document with the
  -- given title.
  | ShowWindowWithTitle String
  -- | A window should be created for the initial document, but it will remain
  -- hidden until made visible by the QML script.
  | HideWindow

-- | Holds parameters for configuring a QML runtime engine.
data EngineConfig a = EngineConfig {
  -- | URL for the first QML document to be loaded.
  initialURL         :: URI,
  -- | Window state for the initial QML document.
  initialWindowState :: InitialWindowState,
  -- | Context 'Object' made available to QML script code.
  contextObject      :: Maybe (ObjRef a)
}

-- | Default engine configuration. Loads @\"main.qml\"@ from the current
-- working directory into a visible window with no context object.
defaultEngineConfig :: EngineConfig a
defaultEngineConfig = EngineConfig {
  initialURL         = nullURI {uriPath = "main.qml"},
  initialWindowState = ShowWindow,
  contextObject      = Nothing
}

isWindowShown :: InitialWindowState -> Bool
isWindowShown ShowWindow = True
isWindowShown (ShowWindowWithTitle _) = True
isWindowShown HideWindow = False

getWindowTitle :: InitialWindowState -> Maybe String
getWindowTitle (ShowWindowWithTitle t) = Just t
getWindowTitle _ = Nothing

-- | Create a QML engine from a specification of its configuration.
createEngine :: (Object a) => EngineConfig a -> IO ()
createEngine config = do
  hsqmlInit
  let hndl = fmap (\(ObjRef h) -> h) $ contextObject config
      url = initialURL config
      state = initialWindowState config
      showWin = isWindowShown state
      maybeTitle = getWindowTitle state
      setTitle = isJust maybeTitle
      titleStr = fromMaybe "" maybeTitle
  mOutAlloc url $ \urlPtr -> do
    mOutAlloc titleStr $ \titlePtr -> do
      hsqmlCreateEngine hndl urlPtr showWin setTitle titlePtr

-- | Enters the Qt event loop and runs until all engines have terminated.
runEngines :: IO ()
runEngines = do
  hsqmlInit
  hsqmlRun
