{-# LANGUAGE DeriveDataTypeable, TypeFamilies #-}

module Graphics.QML.Test.DataTest where

import Graphics.QML.Marshal
import Graphics.QML.Objects
import Graphics.QML.Test.Framework
import Graphics.QML.Test.MayGen
import qualified Graphics.QML.Test.ScriptDSL as S

import Test.QuickCheck.Arbitrary
import Control.Applicative
import Data.Typeable

data DataTest a
    = DTCallMethod a
    | DTMethodRet a
    | DTReadProp a
    | DTWriteProp a
    deriving (Eq, Show, Typeable)

instance (Eq a, Show a, Typeable a, S.Literal a, Arbitrary a, MakeDefault a,
          Marshal a, CanPassTo a ~ Yes, CanReturnTo a ~ Yes, CanGetFrom a ~ Yes)
         => TestAction (DataTest a) where
    legalActionIn _ _ = True 
    nextActionsFor _ = mayOneof [
        DTCallMethod <$> fromGen arbitrary,
        DTMethodRet <$> fromGen arbitrary,
        DTReadProp <$> fromGen arbitrary,
        DTWriteProp <$> fromGen arbitrary]
    updateEnvRaw _ = testEnvStep
    actionRemote (DTCallMethod v) n =
        S.eval $ S.var n `S.dot` "callMethod" `S.call` [S.literal v]
    actionRemote (DTMethodRet v) n =
        S.assert $ S.deepEq (S.var n `S.dot` "methodRet" `S.call` []) $
            S.literal v
    actionRemote (DTReadProp v) n =
        S.assert $ S.deepEq (S.var n `S.dot` "readProp") $ S.literal v
    actionRemote (DTWriteProp v) n =
        S.var n `S.dot` "writeProp" `S.set` S.literal v
    mockObjDef = [
        defMethod "methodRet" $ \m -> expectAction m $ \a -> case a of
            DTMethodRet v -> return $ Right v
            _             -> return $ Left TBadActionCtor,
        defMethod "callMethod" $ \m v ->
            checkAction m (DTCallMethod v) $ return (),
        defPropertyRW "readProp"
            (\m -> expectAction m $ \a -> case a of
                DTReadProp v -> return $ Right v
                _            -> return $ Left TBadActionCtor)
            (\m _ -> badAction m),
        defPropertyRW "writeProp"
            (\_ -> makeDef) (\m v -> checkAction m (DTWriteProp v) $ return ())]
