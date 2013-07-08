{-# LANGUAGE
    DeriveDataTypeable,
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
  runEngine,
  EngineException(),
  filePathToURI
) where

import Graphics.QML.Internal.Marshal
import Graphics.QML.Internal.Objects
import Graphics.QML.Internal.BindCore
import Graphics.QML.Marshal
import Graphics.QML.Objects

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Data.Traversable as T
import Data.Typeable
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
data EngineConfig = EngineConfig {
  -- | URL for the first QML document to be loaded.
  initialURL         :: URI,
  -- | Window state for the initial QML document.
  initialWindowState :: InitialWindowState,
  -- | Context 'Object' made available to QML script code.
  contextObject      :: Maybe AnyObjRef
}

-- | Default engine configuration. Loads @\"main.qml\"@ from the current
-- working directory into a visible window with no context object.
defaultEngineConfig :: EngineConfig
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

runEngineImpl :: EngineConfig -> EngineStopCb -> IO Int
runEngineImpl config stopCb = do
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
            hsqmlRunEngine hndl urlPtr showWin setTitle titlePtr stopCb

-- | Starts a new QML engine using the supplied configuration and blocks at
-- least until the engine has terminated.
--
-- The first time an application runs an engine then the Qt framework will be
-- initialised and its main event loop bound to the current operating system
-- thread. The event loop will run on this thread until all engines have
-- terminated. It's recommended that applications run their first engine on the
-- primordial thread as some platforms prevent running the event loop on other
-- threads. If the event loop fails to start then an 'EngineException' will be
-- thrown.
--
-- This function is thread-safe. Additional engines can be started from
-- arbitrary threads and these calls will block until their respective engines
-- terminate. However, the call used to start the first engine is special
-- because its thread will be used to host the Qt event loop. Hence, it may
-- continue to block after its own engine has terminated, until all running
-- engines have terminated and the event loop can finish.
--
-- Once the event loop has finished, starting a further engine will attempt to
-- start the event loop again, as in the first instance. Any call which may
-- restart the event loop must be made on the same thread used to host it
-- previously, as the event loop thread cannot be changed once Qt has been
-- initialised.
runEngine :: EngineConfig -> IO ()
runEngine config = do
    finishVar <- newEmptyMVar
    let stopCb = putMVar finishVar () 
    ret <- runEngineImpl config stopCb
    if ret /= 0
        then throw EngineException
        else void $ takeMVar finishVar

-- | Exception type used to report errors while starting QML engines.
data EngineException = EngineException deriving (Show, Typeable)

instance Exception EngineException

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
