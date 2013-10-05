{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}

module Graphics.QML.Test.SimpleTest where

import Graphics.QML.Objects
import Graphics.QML.Test.Framework
import Graphics.QML.Test.MayGen
import Graphics.QML.Test.GenURI
import Graphics.QML.Test.ScriptDSL (Expr, Prog)
import qualified Graphics.QML.Test.ScriptDSL as S

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Control.Applicative
import Data.Typeable
import Data.Proxy
import Data.Char
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Numeric

import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI

makeCall :: Int -> String -> [Expr] -> Prog
makeCall n name es = S.eval $ S.var n `S.dot` name `S.call` es

saveCall :: Int -> Int -> String -> [Expr] -> Prog
saveCall v n name es = S.saveVar v $ S.var n `S.dot` name `S.call` es

testCall :: Int -> String -> [Expr] -> Expr -> Prog
testCall n name es r = S.assert $ S.eq (S.var n `S.dot` name `S.call` es) r

setProp :: Int -> String -> Expr -> Prog
setProp n name ex = S.set (S.var n `S.dot` name) ex

saveProp :: Int -> Int -> String -> Prog
saveProp v n name = S.saveVar v $ S.var n `S.dot` name

testProp :: Int -> String -> Expr -> Prog
testProp n name r = S.assert $ S.eq (S.var n `S.dot` name) r

checkArg :: (Show a, Eq a) => a -> a -> IO (Either TestFault ())
checkArg v w = return $
    if v == w then Right () else Left TBadActionData

data SimpleMethods
    = SMTrivial
    | SMGetInt Int32
    | SMSetInt Int32
    | SMGetDouble Double
    | SMSetDouble Double
    | SMGetString String
    | SMSetString String
    | SMGetText Text
    | SMSetText Text
    | SMGetURI URI
    | SMSetURI URI
    | SMGetObject Int
    | SMSetObject Int
    deriving (Show, Typeable)

instance TestAction SimpleMethods where
    legalActionIn (SMSetObject n) env = testEnvIsaJ n testObjectType env
    legalActionIn _ _ = True
    nextActionsFor env = mayOneof [
        pure SMTrivial,
        SMGetInt <$> fromGen arbitrary,
        SMSetInt <$> fromGen arbitrary,
        SMGetDouble <$> fromGen arbitrary,
        SMSetDouble <$> fromGen arbitrary,
        SMGetString <$> fromGen arbitrary,
        SMSetString <$> fromGen arbitrary,
        SMGetText . T.pack <$> fromGen arbitrary,
        SMSetText . T.pack <$> fromGen arbitrary,
        SMGetURI <$> fromGen uriGen,
        SMSetURI <$> fromGen uriGen,
        pure . SMGetObject $ testEnvNextJ env,
        SMSetObject <$> mayElements (testEnvListJ testObjectType env)]
    updateEnvRaw (SMGetObject n) = testEnvStep . testEnvSerial (\s ->
        testEnvSetJ n testObjectType s)
    updateEnvRaw _ = testEnvStep
    actionRemote SMTrivial n = makeCall n "trivial" []
    actionRemote (SMGetInt v) n = testCall n "getInt" [] $ S.literal v
    actionRemote (SMSetInt v) n = makeCall n "setInt" [S.literal v]
    actionRemote (SMGetDouble v) n = testCall n "getDouble" [] $ S.literal v
    actionRemote (SMSetDouble v) n = makeCall n "setDouble" [S.literal v]
    actionRemote (SMGetString v) n = testCall n "getString" [] $ S.literal v
    actionRemote (SMSetString v) n = makeCall n "setString" [S.literal v]
    actionRemote (SMGetText v) n = testCall n "getText" [] $ S.literal v
    actionRemote (SMSetText v) n = makeCall n "setText" [S.literal v]
    actionRemote (SMGetURI v) n = testCall n "getURI" [] $ S.literal v
    actionRemote (SMSetURI v) n = makeCall n "setURI" [S.literal v]
    actionRemote (SMGetObject v) n = saveCall v n "getObject" []
    actionRemote (SMSetObject v) n = makeCall n "setObject" [S.var v]
    mockObjDef = [
        defMethod "trivial" $ \m -> expectAction m $ \a -> case a of
            SMTrivial -> return $ Right ()
            _         -> return $ Left TBadActionCtor,
        defMethod "getInt" $ \m -> expectAction m $ \a -> case a of
            SMGetInt v -> return $ Right v
            _          -> return $ Left TBadActionCtor,
        defMethod "setInt" $ \m v -> expectAction m $ \a -> case a of
            SMSetInt w -> checkArg v w
            _          -> return $ Left TBadActionCtor,
        defMethod "getDouble" $ \m -> expectAction m $ \a -> case a of
            SMGetDouble v -> return $ Right v
            _             -> return $ Left TBadActionCtor,
        defMethod "setDouble" $ \m v -> expectAction m $ \a -> case a of
            SMSetDouble w -> checkArg v w
            _             -> return $ Left TBadActionCtor,
        defMethod "getString" $ \m -> expectAction m $ \a -> case a of
            SMGetString v -> return $ Right v
            _             -> return $ Left TBadActionCtor,
        defMethod "setString" $ \m v -> expectAction m $ \a -> case a of
            SMSetString w -> checkArg v w
            _             -> return $ Left TBadActionCtor,
        defMethod "getText" $ \m -> expectAction m $ \a -> case a of
            SMGetText v -> return $ Right v
            _           -> return $ Left TBadActionCtor,
        defMethod "setText" $ \m v -> expectAction m $ \a -> case a of
            SMSetText w -> checkArg v w
            _           -> return $ Left TBadActionCtor,
        defMethod "getURI" $ \m -> expectAction m $ \a -> case a of
            SMGetURI v -> return $ Right v
            _          -> return $ Left TBadActionCtor,
        defMethod "setURI" $ \m v -> expectAction m $ \a -> case a of
            SMSetURI w -> checkArg v w
            _          -> return $ Left TBadActionCtor,
        defMethod "getObject" $ \m -> expectAction m $ \a -> case a of
            SMGetObject _ -> getTestObject m
            _             -> return $ Left TBadActionCtor,
        defMethod "setObject" $ \m v -> expectAction m $ \a -> case a of
            SMSetObject w -> setTestObject m v w
            _             -> return $ Left TBadActionCtor]

data SimpleProperties
    = SPGetIntRO Int32
    | SPGetInt Int32
    | SPSetInt Int32
    | SPGetDouble Double
    | SPSetDouble Double
    | SPGetString String
    | SPSetString String
    | SPGetText Text
    | SPSetText Text
    | SPGetURI URI
    | SPSetURI URI
    | SPGetObject Int
    | SPSetObject Int
    deriving (Show, Typeable)

instance TestAction SimpleProperties where
    legalActionIn (SPSetObject n) env = testEnvIsaJ n testObjectType env
    legalActionIn _ _ = True
    nextActionsFor env = mayOneof [
        SPGetIntRO <$> fromGen arbitrary,
        SPGetInt <$> fromGen arbitrary,
        SPSetInt <$> fromGen arbitrary,
        SPGetDouble <$> fromGen arbitrary,
        SPSetDouble <$> fromGen arbitrary,
        SPGetString <$> fromGen arbitrary,
        SPSetString <$> fromGen arbitrary,
        SPGetText . T.pack <$> fromGen arbitrary,
        SPSetText . T.pack <$> fromGen arbitrary,
        SPGetURI <$> fromGen uriGen,
        SPSetURI <$> fromGen uriGen,
        pure . SPGetObject $ testEnvNextJ env,
        SPSetObject <$> mayElements (testEnvListJ testObjectType env)]
    updateEnvRaw (SPGetObject n) = testEnvStep . testEnvSerial (\s ->
        testEnvSetJ n testObjectType s)
    updateEnvRaw _ = testEnvStep
    actionRemote (SPGetIntRO v) n = testProp n "propIntRO" $ S.literal v
    actionRemote (SPGetInt v) n = testProp n "propIntR" $ S.literal v
    actionRemote (SPSetInt v) n = setProp n "propIntW" $ S.literal v
    actionRemote (SPGetDouble v) n = testProp n "propDoubleR" $ S.literal v
    actionRemote (SPSetDouble v) n = setProp n "propDoubleW" $ S.literal v
    actionRemote (SPGetString v) n = testProp n "propStringR" $ S.literal v
    actionRemote (SPSetString v) n = setProp n "propStringW" $ S.literal v
    actionRemote (SPGetText v) n = testProp n "propTextR" $ S.literal v
    actionRemote (SPSetText v) n = setProp n "propTextW" $ S.literal v
    actionRemote (SPGetURI v) n = testProp n "propURIR" $ S.literal v
    actionRemote (SPSetURI v) n = setProp n "propURIW" $ S.literal v
    actionRemote (SPGetObject v) n = saveProp v n "propObjectR"
    actionRemote (SPSetObject v) n = setProp n "propObjectW" $ S.var v
    mockObjDef = [
        -- There are seperate properties for testing accessors and mutators
        -- becasue QML produces spurious reads when writing.
        defPropertyRO "propIntRO"
            (\m -> expectAction m $ \a -> case a of
                SPGetIntRO v -> return $ Right v
                _            -> return $ Left TBadActionCtor),
        defPropertyRW "propIntR"
            (\m -> expectAction m $ \a -> case a of
                SPGetInt v -> return $ Right v
                _          -> return $ Left TBadActionCtor)
            (\m _ -> badAction m),
        defPropertyRW "propIntW"
            (\m -> makeDef)
            (\m v -> expectAction m $ \a -> case a of
                SPSetInt w -> checkArg v w
                _          -> return $ Left TBadActionCtor),
        defPropertyRW "propDoubleR"
            (\m -> expectAction m $ \a -> case a of
                SPGetDouble v -> return $ Right v
                _             -> return $ Left TBadActionCtor)
            (\m _ -> badAction m),
        defPropertyRW "propDoubleW"
            (\m -> makeDef)
            (\m v -> expectAction m $ \a -> case a of
                SPSetDouble w -> checkArg v w
                _             -> return $ Left TBadActionCtor),
        defPropertyRW "propStringR"
            (\m -> expectAction m $ \a -> case a of
                SPGetString v -> return $ Right v
                _             -> return $ Left TBadActionCtor)
            (\m _ -> badAction m),
        defPropertyRW "propStringW"
            (\m -> makeDef)
            (\m v -> expectAction m $ \a -> case a of
                SPSetString w -> checkArg v w
                _             -> return $ Left TBadActionCtor),
        defPropertyRW "propTextR"
            (\m -> expectAction m $ \a -> case a of
                SPGetText v -> return $ Right v
                _           -> return $ Left TBadActionCtor)
            (\m _ -> badAction m),
        defPropertyRW "propTextW"
            (\m -> makeDef)
            (\m v -> expectAction m $ \a -> case a of
                SPSetText w -> checkArg v w
                _           -> return $ Left TBadActionCtor),
        defPropertyRW "propURIR"
            (\m -> expectAction m $ \a -> case a of
                SPGetURI v -> return $ Right v
                _          -> return $ Left TBadActionCtor)
            (\m _ -> badAction m),
        defPropertyRW "propURIW"
            (\m -> makeDef)
            (\m v -> expectAction m $ \a -> case a of
                SPSetURI w -> checkArg v w
                _          -> return $ Left TBadActionCtor),
        defPropertyRW "propObjectR"
            (\m -> expectAction m $ \a -> case a of
                SPGetObject _ -> getTestObject m
                _             -> return $ Left TBadActionCtor)
            (\m _ -> badAction m),
        defPropertyRW "propObjectW"
            (\m -> makeDef)
            (\m v -> expectAction m $ \a -> case a of
                SPSetObject w -> setTestObject m (fromObjRef v) w
                _             -> return $ Left TBadActionCtor)]

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
