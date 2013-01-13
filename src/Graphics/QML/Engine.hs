{-# LANGUAGE
    ExistentialQuantification,
    FlexibleContexts
  #-}

-- | Functions for starting QML engines, displaying content in a window.
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
  runEngines,
  filePathToURI
) where

import Graphics.QML.Internal.Marshal
import Graphics.QML.Internal.Objects
import Graphics.QML.Internal.Engine
import Graphics.QML.Marshal
import Graphics.QML.Objects

import Data.List
import Data.Maybe
import Data.Traversable as T
import System.FilePath (isAbsolute, splitDirectories, pathSeparators)
import Network.URI (URI(URI), URIAuth(URIAuth), nullURI, uriPath)

-- | Specifies the intial state of the display window.
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
  contextObject      :: Maybe a
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
createEngine ::
    (Marshal a, MarshalToObj (MarshalMode a)) => EngineConfig a -> IO ()
createEngine config = do
  hsqmlInit
  let obj = contextObject config
      url = initialURL config
      state = initialWindowState config
      showWin = isWindowShown state
      maybeTitle = getWindowTitle state
      setTitle = isJust maybeTitle
      titleStr = fromMaybe "" maybeTitle
  hndl <- T.sequence $ fmap mHsToObj $ obj
  mHsToAlloc url $ \urlPtr -> do
    mHsToAlloc titleStr $ \titlePtr -> do
      hsqmlCreateEngine hndl urlPtr showWin setTitle titlePtr

-- | Enters the Qt event loop and runs until all engines have terminated.
runEngines :: IO ()
runEngines = do
  hsqmlInit
  hsqmlRun

-- | Convenience function for converting local file paths into URIs.
filePathToURI :: FilePath -> URI
filePathToURI fp =
    let ds = splitDirectories fp
        abs = isAbsolute fp
        fixHead =
            (\cs -> if null cs then [] else '/':cs) .
            takeWhile (\c -> not $ c `elem` pathSeparators)
        mapHead _ [] = []
        mapHead f (x:xs) = f x : xs
        afp = intercalate "/" $ mapHead fixHead ds
        rfp = intercalate "/" ds
    in if abs
       then URI "file:" (Just $ URIAuth "" "" "") afp "" ""
       else URI "" Nothing rfp "" ""
