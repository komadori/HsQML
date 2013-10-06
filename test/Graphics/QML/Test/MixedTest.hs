{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}

module Graphics.QML.Test.MixedTest where

import Graphics.QML.Objects
import Graphics.QML.Test.Framework
import Graphics.QML.Test.MayGen
import qualified Graphics.QML.Test.ScriptDSL as S

import Test.QuickCheck.Gen
import Control.Applicative
import Data.Typeable
import Data.Proxy

data ObjectA
    = OANewB Int
    | OACheckB Int
    deriving (Show, Typeable)

objectAType :: TestType
objectAType = TestType (Proxy :: Proxy ObjectA)

data ObjectB
    = OBNewA Int
    | OBCheckA Int
    deriving (Show, Typeable)

objectBType :: TestType
objectBType = TestType (Proxy :: Proxy ObjectB)

instance TestAction ObjectA where
    legalActionIn (OANewB n) env = testEnvIsaJ n objectBType env
    legalActionIn _ _ = True
    nextActionsFor env = mayOneof [
        OANewB <$> (fromGen $ choose (1, testEnvNextJ env)),
        OACheckB <$> mayElements (filter (/= 0) $ testEnvListJ objectBType env)]
    updateEnvRaw (OANewB n) = testEnvStep . testEnvSerial (\s ->
        testEnvSetJ n objectBType s)
    updateEnvRaw _ = testEnvStep
    actionRemote (OANewB v) n =
        S.saveVar v $ S.var n `S.dot` "newB" `S.call` []
    actionRemote (OACheckB v) n =
        S.eval $ S.var n `S.dot` "checkB" `S.call` [S.var v]
    mockObjDef = [
        defMethod "newB" $ \m -> expectAction m $ \a -> case a of
            OANewB _ -> do
                obj <- forkMockObj m
                return $ Right (obj :: ObjRef (MockObj ObjectB))
            _        -> return $ Left TBadActionCtor,
        defMethod "checkB" $ \m v -> expectAction m $ \a -> case a of
            OACheckB w -> checkMockObj m (v :: MockObj ObjectB) w
            _          -> return $ Left TBadActionCtor]

instance TestAction ObjectB where
    legalActionIn (OBNewA n) env = testEnvIsaJ n objectAType env
    legalActionIn _ _ = True
    nextActionsFor env = mayOneof [
        OBNewA <$> (fromGen $ choose (1, testEnvNextJ env)),
        OBCheckA <$> mayElements (filter (/= 0) $ testEnvListJ objectAType env)]
    updateEnvRaw (OBNewA n) = testEnvStep . testEnvSerial (\s ->
        testEnvSetJ n objectAType s)
    updateEnvRaw _ = testEnvStep
    actionRemote (OBNewA v) n =
        S.saveVar v $ S.var n `S.dot` "newA" `S.call` []
    actionRemote (OBCheckA v) n =
        S.eval $ S.var n `S.dot` "checkA" `S.call` [S.var v]
    mockObjDef = [
        defMethod "newA" $ \m -> expectAction m $ \a -> case a of
            OBNewA _ -> do
                obj <- forkMockObj m
                return $ Right (obj :: ObjRef (MockObj ObjectA))
            _        -> return $ Left TBadActionCtor,
        defMethod "checkA" $ \m v -> expectAction m $ \a -> case a of
            OBCheckA w -> checkMockObj m (v :: MockObj ObjectA) w
            _          -> return $ Left TBadActionCtor]
