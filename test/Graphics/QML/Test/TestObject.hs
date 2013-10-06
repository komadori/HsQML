{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}

module Graphics.QML.Test.TestObject where

import Graphics.QML.Objects
import Graphics.QML.Test.Framework
import Graphics.QML.Test.MayGen

import Data.Typeable
import Data.Proxy

data TestObject deriving Typeable

testObjectType :: TestType
testObjectType = TestType (Proxy :: Proxy TestObject)

getTestObject ::
    MockObj a -> IO (Either TestFault (ObjRef (MockObj TestObject)))
getTestObject = fmap Right . forkMockObj

setTestObject ::
    MockObj a -> MockObj TestObject -> Int -> IO (Either TestFault ())
setTestObject = checkMockObj

instance Show TestObject where
    showsPrec _ = error "TestObject has no actions."

instance TestAction TestObject where
    legalActionIn _ _ = error "TestObject has no actions."
    nextActionsFor _ = noGen
    updateEnvRaw _ e = e
    actionRemote _ _ = error "TestObject has no actions."
    mockObjDef = []
