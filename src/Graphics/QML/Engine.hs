{-# LANGUAGE
    ExistentialQuantification,
    Rank2Types
  #-}

module Graphics.QML.Engine (
  InitialWindowState(
    ShowWindow,
    ShowWindowWithTitle,
    HideWindow,
    NoWindow),
  EngineConfig(
    EngineConfig,
    baseURL,
    initialWindowState,
    initialURL,
    contextObject),
  defaultEngineConfig,
  createEngine,
  runEngines
) where

import Graphics.QML.Internal.Core
import Graphics.QML.Internal.Engine
import Graphics.QML.Types.Classes

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
  -- | The inital document should not have a window, but it may create child
  -- windows using QML script.
  | NoWindow

-- | Holds parameters for configuring a QML runtime engine.
data EngineConfig a = EngineConfig {
  -- | Absolute URL against which relative URLs can be resolved.
  baseURL            :: Maybe URI,
  -- | Window state for the initial QML document.
  initialWindowState :: InitialWindowState,
  -- | URL for the first QML document to be loaded.
  initialURL         :: URI,
  -- | Context 'MetaObject' made available to QML script code.
  contextObject      :: Maybe (ObjRef a)
}

-- | Default engine configuration. Loads @\"main.qml\"@ from the current
-- working directory into a visible window with no context object.
defaultEngineConfig :: EngineConfig a
defaultEngineConfig = EngineConfig {
  baseURL            = Nothing,
  initialWindowState = ShowWindow,
  initialURL         = nullURI {uriPath = "main.qml"},
  contextObject      = Nothing
}

-- | Create a QML engine from a specification of its configuration.
createEngine :: (MetaObject a) => EngineConfig a -> IO ()
createEngine config = do
  hsqmlInit
  objPtr <- withMarshal (fromJust $ contextObject config) peek
  hsqmlCreateEngine
    objPtr
    (uriToString id (initialURL config) "")

-- | Enters the Qt event loop and runs until all engines have terminated.
runEngines :: IO ()
runEngines = do
  hsqmlInit
  hsqmlRun
