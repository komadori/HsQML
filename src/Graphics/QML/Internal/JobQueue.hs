module Graphics.QML.Internal.JobQueue (
    postJob,
    processJobs
) where

import Graphics.QML.Internal.BindCore

import Control.Concurrent.MVar
import System.IO.Unsafe

{-# NOINLINE jobQueue #-}
jobQueue :: MVar [IO ()]
jobQueue = unsafePerformIO $ newMVar []

postJob :: IO () -> IO ()
postJob j = do
    notify <- modifyMVar jobQueue $ \js -> return (j:js,null js)
    if notify
    then hsqmlEvloopNotifyJobs 
    else return ()

processJobs :: IO ()
processJobs = do
    js <- modifyMVar jobQueue $ \js -> return ([],js)
    sequence_ $ reverse js
