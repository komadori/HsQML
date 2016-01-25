{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, TypeFamilies #-}

module Graphics.QML.Test.AutoListTest where

import Graphics.QML.Marshal
import Graphics.QML.Objects
import Graphics.QML.Test.Framework
import Graphics.QML.Test.MayGen
import Graphics.QML.Test.TestObject
import Graphics.QML.Test.ScriptDSL (Expr, Prog)
import qualified Graphics.QML.Test.ScriptDSL as S

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Control.Applicative
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable

data Mode = ByReset | ByIndex | ByKey | ByKeyNoReorder
    deriving (Bounded, Enum, Eq, Show, Typeable)

instance Marshal Mode where
    type MarshalMode Mode c d = ModeBidi c
    marshaller = bidiMarshaller toEnum fromEnum

instance S.Literal Mode where
    literal ByReset = S.Expr $ showString "AutoListModel.ByReset"
    literal ByIndex = S.Expr $ showString "AutoListModel.ByIndex"
    literal ByKey = S.Expr $ showString "AutoListModel.ByKey"
    literal ByKeyNoReorder = S.Expr $ showString "AutoListModel.ByKeyNoReorder"

data AutoListTest
    = CreateAutoList Int
    deriving (Eq, Show, Typeable)

data AutoList
    = SetSource [Int]
    | SetMode Mode
    deriving (Eq, Show, Typeable)

qtN, hsN :: Int -> Int
qtN n = 2*n+0
hsN n = 2*n+1

autoListType :: TestType
autoListType = TestType (Proxy :: Proxy AutoList)

testCode :: Text
testCode = T.pack $ unlines [
    "import QtQuick 2.0;",
    "import HsQML.Model 1.0;",
    "Item {",
    "    id: obj;",
    "    Repeater {",
    "        id: rep;",
    "        model: AutoListModel { onSourceChanged: tmr.start(); }",
    "        Item { property var value : modelData; }",
    "    }",
    "    property var model : rep.model;",
    "    Timer { id: tmr; interval: 50; onTriggered: obj.ready(); }",
    "    signal ready;",
    "    function get() {",
    "        var xs = [];",
    "        for (var i=0; i<rep.count; i++) {xs.push(rep.itemAt(i).value);}",
    "        return xs;",
    "    }",
    "}"]

instance TestAction AutoListTest where
    legalActionIn _ _ = True
    nextActionsFor env = pure . CreateAutoList $ testEnvNextJ env
    updateEnvRaw (CreateAutoList n) = testEnvStep . testEnvSerial (\s ->
        testEnvSetJ n autoListType s)
    actionRemote (CreateAutoList n) _ =
        (S.saveVar (qtN n) (S.call (S.sym "Qt" `S.dot` "createQmlObject")
            [S.literal testCode, S.sym "page", S.literal $ T.pack "dyn"])) <>
        (S.saveVar (hsN n) (S.call (S.sym "create") [S.literal n]))
    mockObjDef = [
        defMethod "create" $ \m n -> checkAction m (CreateAutoList n)
            (forkMockObj m :: IO (ObjRef (MockObj AutoList)))]

instance TestAction AutoList where
    legalActionIn _ _ = True
    nextActionsFor env = fromGen $ oneof [
        SetSource <$> arbitrary,
        SetMode <$> arbitraryBoundedEnum]
    updateEnvRaw _ = testEnvStep
    actionRemote (SetSource xs) n =
        S.makeCont [] (
            S.connect (S.var (qtN n) `S.dot` "ready") S.contVar <>
            S.set (S.var (qtN n) `S.dot` "model" `S.dot` "source")
                (S.literal xs)) <>
        S.disconnect (S.var (qtN n) `S.dot` "ready") S.callee <>
        S.eval (S.call (S.var (hsN n) `S.dot` "sourceSet") [
            S.call (S.var (qtN n) `S.dot` "get") []])
    actionRemote (SetMode x) n =
        S.set (S.var (qtN n) `S.dot` "model" `S.dot` "mode") (S.literal x) <>
        S.eval (S.call (S.var (hsN n) `S.dot` "modeSet") [S.literal x])
    mockObjDef = [
        defMethod "sourceSet" $ \m xs ->
            checkAction m (SetSource xs) $ return (),
        defMethod "modeSet" $ \m x ->
            checkAction m (SetMode x) $ return ()]
