{-# LANGUAGE
    DeriveDataTypeable,
    FlexibleContexts,
    GeneralizedNewtypeDeriving
  #-}

-- | Functions for starting QML engines, displaying content in a window.
module Graphics.QML.Engine (
  -- * Engines
  EngineConfig(
    EngineConfig,
    initialDocument,
    contextObject,
    importPaths,
    pluginPaths),
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
  shutdownQt,
  EventLoopException(),

  -- * Document Paths
  DocumentPath(),
  fileDocument,
  uriDocument
) where

import Graphics.QML.Internal.JobQueue
import Graphics.QML.Internal.Marshal
import Graphics.QML.Internal.BindPrim
import Graphics.QML.Internal.BindCore
import Graphics.QML.Marshal ()
import Graphics.QML.Objects

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.List
import Data.Traversable
import Data.Typeable
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.FilePath (FilePath, isAbsolute, splitDirectories, pathSeparators)

-- | Holds parameters for configuring a QML runtime engine.
data EngineConfig = EngineConfig {
  -- | Path to the first QML document to be loaded.
  initialDocument    :: DocumentPath,
  -- | Context 'Object' made available to QML script code.
  contextObject      :: Maybe AnyObjRef,
  -- | Additional search paths for QML modules
  importPaths        :: [FilePath],
  -- | Additional search paths for QML native plugins
  pluginPaths        :: [FilePath]
}

-- | Default engine configuration. Loads @\"main.qml\"@ from the current
-- working directory into a visible window with no context object.
defaultEngineConfig :: EngineConfig
defaultEngineConfig = EngineConfig {
  initialDocument    = DocumentPath "main.qml",
  contextObject      = Nothing,
  importPaths        = [],
  pluginPaths        = []
}

-- | Represents a QML engine.
data Engine = Engine

runEngineImpl :: EngineConfig -> IO () -> IO Engine
runEngineImpl config stopCb = do
    hsqmlInit
    let obj = contextObject config
        DocumentPath res = initialDocument config
        impPaths = importPaths config
        plugPaths = pluginPaths config
    hndl <- sequenceA $ fmap mToHndl obj
    mWithCVal (T.pack res) $ \resPtr ->
        withManyArray0 mWithCVal (map T.pack impPaths) nullPtr $ \impPtr ->
        withManyArray0 mWithCVal (map T.pack plugPaths) nullPtr $ \plugPtr ->
            hsqmlCreateEngine hndl (HsQMLStringHandle $ castPtr resPtr)
                (castPtr impPtr) (castPtr plugPtr) stopCb
    return Engine

withMany :: (a -> (b -> m c) -> m c) -> [a] -> ([b] -> m c) -> m c
withMany func as cont =
    let rec (a:as') bs = func a (\b -> rec as' (bs . (b:)))
        rec []      bs = cont $ bs []
    in rec as id

withManyArray0 :: Storable b =>
    (a -> (b -> IO c) -> IO c) -> [a] -> b -> (Ptr b -> IO c) -> IO c
withManyArray0 func as term cont =
    withMany func as $ \ptrs -> withArray0 term ptrs cont

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
runEngineAsync config = RunQML $ runEngineImpl config (return ())

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
    liftIO = RunQML

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
runEventLoop (RunQML runFn) = tryRunInBoundThread $ do
    hsqmlInit
    finishVar <- newEmptyMVar
    let startCb = void $ forkIO $ do
            ret <- try runFn
            case ret of
                Left ex -> putMVar finishVar $ throwIO (ex :: SomeException)
                Right ret' -> putMVar finishVar $ return ret'
            hsqmlEvloopRelease
        yieldCb = if rtsSupportsBoundThreads
                  then Nothing
                  else Just yield
    status <- hsqmlEvloopRun startCb processJobs yieldCb
    case statusException status of
        Just ex -> throw ex
        Nothing -> do 
            finFn <- takeMVar finishVar
            finFn

tryRunInBoundThread :: IO a -> IO a
tryRunInBoundThread action =
    if rtsSupportsBoundThreads
    then runInBoundThread action
    else action

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

-- | Shuts down and frees resources used by the Qt framework, preventing
-- further use of the event loop. The framework is initialised when
-- 'runEventLoop' is first called and remains initialised afterwards so that
-- the event loop can be reentered if desired (e.g. when using GHCi). Once
-- shut down, the framework cannot be reinitialised.
--
-- It is recommended that you call this function at the end of your program as
-- this library will try, but cannot guarantee in all configurations to be able
-- to shut it down for you. Failing to shutdown the framework has been known to
-- intermittently cause crashes on process exit on some platforms.
--
-- This function must be called from the event loop thread and the event loop
-- must not be running at the time otherwise an 'EventLoopException' will be
-- thrown.
shutdownQt :: IO ()
shutdownQt = do
    status <- hsqmlEvloopShutdown
    case statusException status of
        Just ex -> throw ex
        Nothing -> return ()

statusException :: HsQMLEventLoopStatus -> Maybe EventLoopException
statusException HsqmlEvloopOk = Nothing
statusException HsqmlEvloopAlreadyRunning = Just EventLoopAlreadyRunning
statusException HsqmlEvloopPostShutdown = Just EventLoopPostShutdown
statusException HsqmlEvloopWrongThread = Just EventLoopWrongThread
statusException HsqmlEvloopNotRunning = Just EventLoopNotRunning
statusException _ = Just EventLoopOtherError

-- | Exception type used to report errors pertaining to the event loop.
data EventLoopException
    = EventLoopAlreadyRunning
    | EventLoopPostShutdown
    | EventLoopWrongThread
    | EventLoopNotRunning
    | EventLoopOtherError
    deriving (Show, Typeable)

instance Exception EventLoopException

-- | Path to a QML document file.
newtype DocumentPath = DocumentPath String

-- | Converts a local file path into a 'DocumentPath'.
fileDocument :: FilePath -> DocumentPath
fileDocument fp =
    let ds = splitDirectories fp
        isAbs = isAbsolute fp
        fixHead =
            (\cs -> if null cs then [] else '/':cs) .
            takeWhile (`notElem` pathSeparators)
        mapHead _ [] = []
        mapHead f (x:xs) = f x : xs
        afp = intercalate "/" $ mapHead fixHead ds
        rfp = intercalate "/" ds
    in DocumentPath $ if isAbs then "file://" ++ afp else rfp

-- | Converts a URI string into a 'DocumentPath'.
uriDocument :: String -> DocumentPath
uriDocument = DocumentPath
