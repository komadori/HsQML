{-# LANGUAGE
    DeriveDataTypeable,
    FlexibleContexts,
    GeneralizedNewtypeDeriving
  #-}

-- | Functions for starting QML engines, displaying content in a window.
module Graphics.QML.Engine (
  -- * Engines
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
  Engine,
  runEngine,
  runEngineWith,
  runEngineAsync,
  runEngineLoop,

  -- * Event Loop
  RunQML(),
  runEventLoop,
  requireEventLoop,
  EventLoopException(),

  -- * Utilities
  filePathToURI
) where

import Graphics.QML.Internal.Marshal
import Graphics.QML.Internal.Objects
import Graphics.QML.Internal.BindCore
import Graphics.QML.Marshal
import Graphics.QML.Objects

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
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

-- | Represents a QML engine.
data Engine = Engine

runEngineImpl :: EngineConfig -> IO () -> IO Engine
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
            hsqmlCreateEngine hndl urlPtr showWin setTitle titlePtr stopCb
    return Engine

-- | Starts a new QML engine using the supplied configuration and blocks until
-- the engine has terminated.
runEngine :: EngineConfig -> RunQML ()
runEngine config = runEngineWith config (const $ return ())

-- | Starts a new QML engine using the supplied configuration. The \'with\'
-- function is executed once the engine has been started and after it returns
-- this function blocks until the engine has terminated.
runEngineWith :: EngineConfig -> (Engine -> RunQML a) -> RunQML a
runEngineWith config with = RunQML $ do
    finishVar <- newEmptyMVar
    let stopCb = putMVar finishVar () 
    eng <- runEngineImpl config stopCb
    let (RunQML withIO) = with eng
    ret <- withIO
    void $ takeMVar finishVar
    return ret

-- | Starts a new QML engine using the supplied configuration and returns
-- immediately without blocking.
runEngineAsync :: EngineConfig -> RunQML Engine
runEngineAsync config = RunQML $ do
    runEngineImpl config (return ())

-- | Conveniance function that both runs the event loop and starts a new QML
-- engine. It blocks keeping the event loop running until the engine has
-- terminated.
runEngineLoop :: EngineConfig -> IO ()
runEngineLoop config =
    runEventLoop $ runEngine config

-- | Wrapper around the IO monad for running actions which depend on the Qt
-- event loop.
newtype RunQML a = RunQML (IO a) deriving (Functor, Applicative, Monad)

instance MonadIO RunQML where
    liftIO io = RunQML io

-- | This function enters the Qt event loop and executes the supplied function
-- in the 'RunQML' monad on a new unbound thread. The event loop will continue
-- to run until all functions in the 'RunQML' monad have completed. This
-- includes both the 'RunQML' function launched by this call and any launched
-- asynchronously via 'requireEventLoop'. When the event loop exits, all
-- engines will be terminated.
--
-- It's recommended that applications run the event loop on their primordial
-- thread as some platforms mandate this. Once the event loop has finished, it
-- can be started again, but only on the same operating system thread as
-- before. If the event loop fails to start then an 'EventLoopException' will
-- be thrown.
runEventLoop :: RunQML a -> IO a
runEventLoop (RunQML runFn) = do
    hsqmlInit
    finishVar <- newEmptyMVar
    let startCb = void $ forkIO $ do
            ret <- try runFn
            case ret of
                Left ex -> putMVar finishVar $ throwIO (ex :: SomeException)
                Right ret -> putMVar finishVar $ return ret
            hsqmlEvloopRelease
        yieldCb = if rtsSupportsBoundThreads
                  then Nothing
                  else Just yield
    status <- hsqmlEvloopRun startCb yieldCb
    case statusException status of
        Just ex -> throw ex
        Nothing -> do 
            finFn <- takeMVar finishVar
            finFn

-- | Executes a function in the 'RunQML' monad asynchronously to the event
-- loop. Callers must apply their own sychronisation to ensure that the event
-- loop is currently running when this function is called, otherwise an
-- 'EventLoopException' will be thrown. The event loop will not exit until the
-- supplied function has completed.
requireEventLoop :: RunQML a -> IO a
requireEventLoop (RunQML runFn) = do
    hsqmlInit
    let reqFn = do
            status <- hsqmlEvloopRequire
            case statusException status of
                Just ex -> throw ex
                Nothing -> return ()
    bracket_ reqFn hsqmlEvloopRelease runFn

statusException :: HsQMLEventLoopStatus -> Maybe EventLoopException
statusException HsqmlEvloopOk = Nothing
statusException HsqmlEvloopAlreadyRunning = Just EventLoopAlreadyRunning
statusException HsqmlEvloopWrongThread = Just EventLoopWrongThread
statusException HsqmlEvloopNotRunning = Just EventLoopNotRunning
statusException _ = Just EventLoopOtherError

-- | Exception type used to report errors pertaining to the event loop.
data EventLoopException
    = EventLoopAlreadyRunning
    | EventLoopWrongThread
    | EventLoopNotRunning
    | EventLoopOtherError
    deriving (Show, Typeable)

instance Exception EventLoopException

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
