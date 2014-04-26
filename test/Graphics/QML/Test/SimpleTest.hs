{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}

module Graphics.QML.Test.SimpleTest where

import Graphics.QML.Objects
import Graphics.QML.Test.Framework
import Graphics.QML.Test.MayGen
import Graphics.QML.Test.TestObject
import Graphics.QML.Test.ScriptDSL (Expr, Prog)
import qualified Graphics.QML.Test.ScriptDSL as S

import Test.QuickCheck.Arbitrary
import Control.Applicative
import Data.Typeable

import Data.Int
import Data.Text (Text)
import qualified Data.Text as T

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

retVoid :: IO ()
retVoid = return ()

data SimpleMethods
    = SMTrivial
    | SMTernary Int32 Int32 Int32 Int32
    | SMGetInt Int32
    | SMSetInt Int32
    | SMGetDouble Double
    | SMSetDouble Double
    | SMGetText Text
    | SMSetText Text
    | SMGetObject Int
    | SMSetObject Int
    deriving (Eq, Show, Typeable)

instance TestAction SimpleMethods where
    legalActionIn (SMSetObject n) env = testEnvIsaJ n testObjectType env
    legalActionIn _ _ = True
    nextActionsFor env = mayOneof [
        pure SMTrivial,
        SMTernary <$> 
            fromGen arbitrary <*> fromGen arbitrary <*>
            fromGen arbitrary <*> fromGen arbitrary,
        SMGetInt <$> fromGen arbitrary,
        SMSetInt <$> fromGen arbitrary,
        SMGetDouble <$> fromGen arbitrary,
        SMSetDouble <$> fromGen arbitrary,
        SMGetText . T.pack <$> fromGen arbitrary,
        SMSetText . T.pack <$> fromGen arbitrary,
        pure . SMGetObject $ testEnvNextJ env,
        SMSetObject <$> mayElements (testEnvListJ testObjectType env)]
    updateEnvRaw (SMGetObject n) = testEnvStep . testEnvSerial (\s ->
        testEnvSetJ n testObjectType s)
    updateEnvRaw _ = testEnvStep
    actionRemote SMTrivial n = makeCall n "trivial" []
    actionRemote (SMTernary v1 v2 v3 v4) n = testCall n "ternary" [
        S.literal v1, S.literal v2, S.literal v3] $ S.literal v4
    actionRemote (SMGetInt v) n = testCall n "getInt" [] $ S.literal v
    actionRemote (SMSetInt v) n = makeCall n "setInt" [S.literal v]
    actionRemote (SMGetDouble v) n = testCall n "getDouble" [] $ S.literal v
    actionRemote (SMSetDouble v) n = makeCall n "setDouble" [S.literal v]
    actionRemote (SMGetText v) n = testCall n "getText" [] $ S.literal v
    actionRemote (SMSetText v) n = makeCall n "setText" [S.literal v]
    actionRemote (SMGetObject v) n = saveCall v n "getObject" []
    actionRemote (SMSetObject v) n = makeCall n "setObject" [S.var v]
    mockObjDef = [
        defMethod "trivial" $ \m -> checkAction m SMTrivial retVoid,
        defMethod "ternary" $ \m v1 v2 v3 -> expectAction m $ \a -> case a of
            SMTernary w1 w2 w3 w4 ->
                (fmap . fmap) (const w4) $ checkArg (v1,v2,v3) (w1,w2,w3)
            _          -> return $ Left TBadActionCtor,
        defMethod "getInt" $ \m -> expectAction m $ \a -> case a of
            SMGetInt v -> return $ Right v
            _          -> return $ Left TBadActionCtor,
        defMethod "setInt" $ \m v -> checkAction m (SMSetInt v) retVoid,
        defMethod "getDouble" $ \m -> expectAction m $ \a -> case a of
            SMGetDouble v -> return $ Right v
            _             -> return $ Left TBadActionCtor,
        defMethod "setDouble" $ \m v -> checkAction m (SMSetDouble v) retVoid,
        defMethod "getText" $ \m -> expectAction m $ \a -> case a of
            SMGetText v -> return $ Right v
            _           -> return $ Left TBadActionCtor,
        defMethod "setText" $ \m v -> checkAction m (SMSetText v) retVoid,
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
    | SPGetText Text
    | SPSetText Text
    | SPGetObject Int
    | SPSetObject Int
    deriving (Eq, Show, Typeable)

instance TestAction SimpleProperties where
    legalActionIn (SPSetObject n) env = testEnvIsaJ n testObjectType env
    legalActionIn _ _ = True
    nextActionsFor env = mayOneof [
        SPGetIntRO <$> fromGen arbitrary,
        SPGetInt <$> fromGen arbitrary,
        SPSetInt <$> fromGen arbitrary,
        SPGetDouble <$> fromGen arbitrary,
        SPSetDouble <$> fromGen arbitrary,
        SPGetText . T.pack <$> fromGen arbitrary,
        SPSetText . T.pack <$> fromGen arbitrary,
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
    actionRemote (SPGetText v) n = testProp n "propTextR" $ S.literal v
    actionRemote (SPSetText v) n = setProp n "propTextW" $ S.literal v
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
            (\_ -> makeDef) (\m v -> checkAction m (SPSetInt v) retVoid),
        defPropertyRW "propDoubleR"
            (\m -> expectAction m $ \a -> case a of
                SPGetDouble v -> return $ Right v
                _             -> return $ Left TBadActionCtor)
            (\m _ -> badAction m),
        defPropertyRW "propDoubleW"
            (\_ -> makeDef) (\m v -> checkAction m (SPSetDouble v) retVoid),
        defPropertyRW "propTextR"
            (\m -> expectAction m $ \a -> case a of
                SPGetText v -> return $ Right v
                _           -> return $ Left TBadActionCtor)
            (\m _ -> badAction m),
        defPropertyRW "propTextW"
            (\_ -> makeDef) (\m v -> checkAction m (SPSetText v) retVoid),
        defPropertyRW "propObjectR"
            (\m -> expectAction m $ \a -> case a of
                SPGetObject _ -> getTestObject m
                _             -> return $ Left TBadActionCtor)
            (\m _ -> badAction m),
        defPropertyRW "propObjectW"
            (\_ -> makeDef)
            (\m v -> expectAction m $ \a -> case a of
                SPSetObject w -> setTestObject m (fromObjRef v) w
                _             -> return $ Left TBadActionCtor)]
