{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}

module Graphics.QML.Test.TestObject where

import Graphics.QML.Objects
import Graphics.QML.Test.Framework
import Graphics.QML.Test.MayGen

import Data.Typeable
import Data.Proxy
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

data TestObject deriving Typeable

testObjectType :: TestType
testObjectType = TestType (Proxy :: Proxy TestObject)

getTestObject ::
    MockObj a -> IO (Either TestFault (ObjRef (MockObj TestObject)))
getTestObject m = do
    status <- mockGetStatus m
    obj <- newObject (MockObj (testSerial status) $
        mockStatus m :: MockObj TestObject)
    return $ Right obj

setTestObject :: 
    MockObj a -> MockObj TestObject -> Int -> IO (Either TestFault ())
setTestObject m v w = do
    status <- mockGetStatus m
    case IntMap.lookup w $ envJs $ testEnv status of
        Just entry -> if entry == (testObjectType, mockSerial v)
                      then return $ Right ()
                      else return $ Left TBadActionData
        _          -> return $ Left TBadActionSlot

instance Show TestObject where
    showsPrec _ = error "TestObject has no actions."

instance TestAction TestObject where
    legalActionIn _ _ = error "TestObject has no actions."
    nextActionsFor _ = noGen
    updateEnvRaw _ e = e
    actionRemote _ _ = error "TestObject has no actions."
    mockObjDef = []
