{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Graphics.QML
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import Data.Typeable
import Network.URI
import System.Exit
import System.IO
import System.Directory

data HarnessObject = HarnessObject {
    globalState :: IORef [String]
} deriving Typeable

markTaskComplete :: HarnessObject -> String -> IO ()
markTaskComplete go task = do
    let ref = globalState go
    list <- readIORef ref
    if elem task list
    then do
        putStrLn (
            showString "Completed task '" .
            showString task $ showString "'." [])
        writeIORef ref $ delete task list
    else do
        putStrLn (showString "Unexpected task '" .
            showString task $ showString "' completed." [])
        writeIORef ref $ ("unexpected_" ++ task) : list

testInt :: Int
testInt = 8888

testDouble :: Double
testDouble = 0.25

testString :: String
testString = "我係QuickBrownFox"

testURI :: URI
testURI =
    URI "http:" (Just $ URIAuth "" "www.example.com" "")
        "/with space" "" "#test"

data TestObject = TestObject deriving (Eq, Typeable)

instance Object TestObject where
    classDef = defClass []

instance MarshalIn TestObject where
    mIn = objectInMarshaller

instance Object HarnessObject where
    classDef = defClass [
        defMethod "getInt" (\go -> do
            markTaskComplete (fromObjRef go) "getInt"
            return testInt),
        defMethod "setInt" (\go i -> do
            markTaskComplete (fromObjRef go) "setInt"
            if (i == testInt)
            then markTaskComplete (fromObjRef go) "setIntCorrect"
            else return ()),
        defMethod "getDouble" (\go -> do
            markTaskComplete (fromObjRef go) "getDouble"
            return testDouble),
        defMethod "setDouble" (\go i -> do
            markTaskComplete (fromObjRef go) "setDouble"
            if (i == testDouble)
            then markTaskComplete (fromObjRef go) "setDoubleCorrect"
            else return ()),
        defMethod "getString" (\go -> do
            markTaskComplete (fromObjRef go) "getString"
            return testString),
        defMethod "setString" (\go i -> do
            markTaskComplete (fromObjRef go) "setString"
            if (i == testString)
            then markTaskComplete (fromObjRef go) "setStringCorrect"
            else return ()),
        defMethod "getURI" (\go -> do
            markTaskComplete (fromObjRef go) "getURI"
            return testURI),
        defMethod "setURI" (\go i -> do
            markTaskComplete (fromObjRef go) "setURI"
            if (i == testURI)
            then markTaskComplete (fromObjRef go) "setURICorrect"
            else return ()),
        defMethod "getObject" (\go -> do
            markTaskComplete (fromObjRef go) "getObject"
            newObject TestObject),
        defMethod "setObject" (\go i -> do
            markTaskComplete (fromObjRef go) "setObject"
            if (i == TestObject)
            then markTaskComplete (fromObjRef go) "setObjectCorrect"
            else return ())]

testScript :: String
testScript = unlines [
    "import Qt 4.7",
    "Rectangle {",
    "    id: page;",
    "    width: 100; height: 100;",
    "    color: 'green';",
    "    Component.onCompleted: {",
    "        var intVar = getInt();",
    "        setInt(intVar);",
    "        var dblVar = getDouble();",
    "        setDouble(dblVar);",
    "        var strVar = getString();",
    "        setString(strVar);",
    "        var uriVar = getURI();",
    "        setURI(uriVar);",
    "        var objVar = getObject();",
    "        setObject(objVar);",
    "        window.close();",
    "    }",
    "}"]

testTasks :: [String]
testTasks = [
    "getInt", "setInt", "setIntCorrect",
    "getDouble", "setDouble", "setDoubleCorrect",
    "getString", "setString", "setStringCorrect",
    "getURI", "setURI", "setURICorrect",
    "getObject", "setObject", "setObjectCorrect"]

main :: IO ()
main = do
    tmpDir <- getTemporaryDirectory
    (qmlPath, hndl) <- openTempFile tmpDir "test1-.qml"
    hPutStr hndl testScript
    hClose hndl
    list <- newIORef testTasks
    go <- newObject $ HarnessObject list
    createEngine defaultEngineConfig {
        initialURL = filePathToURI qmlPath,
        contextObject = Just go}
    runEngines
    removeFile qmlPath
    ts <- readIORef list
    forM_ ts (\t -> putStrLn (
        showString "Incomplete task '" . showString t $ showString "'." []))
    if null ts
    then exitSuccess
    else exitFailure
