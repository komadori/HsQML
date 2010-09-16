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
    initialURL),
  defaultEngineConfig,
  createEngine,
  runEngines
) where

import Graphics.QML.Internal.Core
import Graphics.QML.Internal.Engine
import Graphics.QML.Types.Classes

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
data EngineConfig = EngineConfig {
  -- | Absolute URL against which relative URLs can be resolved.
  baseURL            :: Maybe URI,
  -- | Window state for the initial QML document.
  initialWindowState :: InitialWindowState,
  -- | URL for the first QML document to be loaded.
  initialURL         :: URI
}

data EmptyObject = EmptyObject ()

instance MetaObject EmptyObject where
  metaClass = mkClass "EmptyObject" []

-- | Default engine configuration. Loads @\"main.qml\"@ from the current
-- working directory into a visible window.
defaultEngineConfig :: EngineConfig
defaultEngineConfig = EngineConfig {
  baseURL            = Nothing,
  initialWindowState = ShowWindow,
  initialURL         = nullURI {uriPath = "main.qml"}
}

-- | Create a QML engine from a specification of its configuration.
createEngine :: forall a. (MetaObject a) => a -> EngineConfig -> IO ()
createEngine obj config = do
  objPtr <- withMarshal obj peek
  hsqmlCreateEngine
    objPtr
    (uriToString id (initialURL config) "")

-- | Enters the Qt event loop and runs until all engines have terminated.
runEngines :: IO ()
runEngines =
  hsqmlRun
