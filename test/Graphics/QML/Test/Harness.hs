module Graphics.QML.Test.Harness where

import Graphics.QML.Test.Framework

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Test

import Graphics.QML
import Data.IORef
import Data.Proxy
import Data.Typeable
import Data.Maybe
import System.IO
import System.Directory

qmlPrelude :: String
qmlPrelude = unlines [
    "import Qt 4.7",
    "Rectangle {",
    "    id: page;",
    "    width: 100; height: 100;",
    "    color: 'green';",
    "    Component.onCompleted: {"]

qmlPostscript :: String
qmlPostscript = unlines [
    "    }",
    "}"]

finishTest :: MockObj a -> IO ()
finishTest mock = do
    let statusRef = mockStatus mock
    status <- readIORef statusRef
    let status' = case status of
            TestStatus (_:_) Nothing _ _ -> status {
                testFault = Just TUnderAction}
            _                            -> status
    writeIORef statusRef status'

runTest :: (TestAction a) => TestBoxSrc a -> IO TestStatus
runTest src = do
    let js = showTestCode (srcTestBoxes src) ""
    tmpDir <- getTemporaryDirectory
    (qmlPath, hndl) <- openTempFile tmpDir "test1-.qml"
    hPutStr hndl (qmlPrelude ++ js ++ qmlPostscript)
    hClose hndl
    mock <- mockFromSrc src
    go <- newObject mock
    runEngineLoop defaultEngineConfig {
        initialURL = filePathToURI qmlPath,
        initialWindowState = HideWindow,
        contextObject = Just $ anyObjRef go}
    removeFile qmlPath
    finishTest mock
    status <- readIORef (mockStatus mock)
    if isJust $ testFault status
        then putStrLn $ show status
        else return ()
    return status

testProperty :: (TestAction a) => TestBoxSrc a -> Property
testProperty src = monadicIO $ do
    status <- run $ runTest src
    assert $ isNothing $ testFault status
    return ()

checkProperty :: TestType -> IO Bool
checkProperty (TestType pxy) = do
    putStrLn $ "Checking " ++ show (typeOf $ asProxyTypeOf undefined pxy)
    r <- quickCheckResult $ testProperty . constrainSrc pxy
    return $ isSuccess r

constrainSrc :: (TestAction a) => Proxy a -> TestBoxSrc a -> TestBoxSrc a
constrainSrc = flip const
