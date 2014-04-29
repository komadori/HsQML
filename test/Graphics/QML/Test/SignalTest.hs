{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, TypeFamilies #-}

module Graphics.QML.Test.SignalTest where

import Graphics.QML.Objects
import Graphics.QML.Test.Framework
import Graphics.QML.Test.MayGen
import Graphics.QML.Test.TestObject
import Graphics.QML.Test.ScriptDSL (Expr, Prog)
import qualified Graphics.QML.Test.ScriptDSL as S

import Test.QuickCheck.Arbitrary
import Control.Applicative
import Data.Monoid
import Data.Proxy
import Data.Typeable

import Data.Int
import Data.Text (Text)
import qualified Data.Text as T

data SignalTest1
    = ST1TrivialMethod
    | ST1FireNoArgs
    | ST1FireInt Int32
    | ST1FireThreeInts Int32 Int32 Int32
    | ST1FireDouble Double
    | ST1FireText Text
    | ST1FireObject Int
    | ST1CheckObject Int
    deriving (Show, Typeable)

data NoArgsSignal deriving Typeable

instance SignalKeyClass NoArgsSignal where
    type SignalParams NoArgsSignal = IO ()

data IntSignal deriving Typeable

instance SignalKeyClass IntSignal where
    type SignalParams IntSignal = Int32 -> IO ()

data ThreeIntsSignal deriving Typeable

instance SignalKeyClass ThreeIntsSignal where
    type SignalParams ThreeIntsSignal = Int32 -> Int32 -> Int32 -> IO ()

data DoubleSignal deriving Typeable

instance SignalKeyClass DoubleSignal where
    type SignalParams DoubleSignal = Double -> IO ()

data TextSignal deriving Typeable

instance SignalKeyClass TextSignal where
    type SignalParams TextSignal = Text -> IO ()

data ObjectSignal deriving Typeable

instance SignalKeyClass ObjectSignal where
    type SignalParams ObjectSignal = ObjRef (MockObj TestObject) -> IO ()

chainSignal :: Int -> [String] -> String -> String -> Prog
chainSignal n args sig fn = S.makeCont args
    ((S.connect (S.var n `S.dot` sig) S.contVar) `mappend` 
     (S.eval $ (S.var n) `S.dot` fn `S.call` []))
    (S.disconnect (S.var n `S.dot` sig) S.callee)

testSignal :: Int -> String -> String -> Expr -> Prog
testSignal n sig fn v =
    chainSignal n ["arg"] sig fn `mappend`
    (S.assert $ S.sym "arg" `S.eq` v)

instance TestAction SignalTest1 where
    legalActionIn _ _ = True
    nextActionsFor env = mayOneof [
        pure ST1TrivialMethod,
        pure ST1FireNoArgs,
        ST1FireInt <$> fromGen arbitrary,
        ST1FireThreeInts <$>
            fromGen arbitrary <*> fromGen arbitrary <*> fromGen arbitrary,
        ST1FireDouble <$> fromGen arbitrary,
        ST1FireText . T.pack <$> fromGen arbitrary,
        pure . ST1FireObject $ testEnvNextJ env,
        ST1CheckObject <$> mayElements (testEnvListJ testObjectType env)]
    updateEnvRaw (ST1FireObject n) = testEnvStep . testEnvSerial (\s ->
        testEnvSetJ n testObjectType s)
    updateEnvRaw _ = testEnvStep
    actionRemote ST1TrivialMethod n =
        S.eval $ (S.var n) `S.dot` "trivialMethod" `S.call` []
    actionRemote ST1FireNoArgs n =
        chainSignal n [] "noArgsSignal" "fireNoArgs"
    actionRemote (ST1FireInt v) n =
        testSignal n "intSignal" "fireInt" $ S.literal v
    actionRemote (ST1FireThreeInts v1 v2 v3) n =
        chainSignal n ["arg1","arg2","arg3"]
            "threeIntsSignal" "fireThreeInts" `mappend`
        (S.assert $ S.sym "arg1" `S.eq` S.literal v1) `mappend`
        (S.assert $ S.sym "arg2" `S.eq` S.literal v2) `mappend`
        (S.assert $ S.sym "arg3" `S.eq` S.literal v3)
    actionRemote (ST1FireDouble v) n =
        testSignal n "doubleSignal" "fireDouble" $ S.literal v
    actionRemote (ST1FireText v) n =
        testSignal n "textSignal" "fireText" $ S.literal v
    actionRemote (ST1FireObject v) n =
        chainSignal n ["obj"] "objectSignal" "fireObject" `mappend`
        (S.saveVar v $ S.sym "obj")
    actionRemote (ST1CheckObject v) n =
        S.eval $ (S.var n) `S.dot` "checkObject" `S.call` [S.var v]
    mockObjDef = [
        defMethod "trivialMethod" $ \m -> expectAction m $ \a -> case a of
            ST1TrivialMethod -> return $ Right ()
            _                -> return $ Left TBadActionCtor,
        defMethod "fireNoArgs" $ \m -> (expectActionRef m $ \a -> case a of
            ST1FireNoArgs -> do
                fireSignal (Proxy :: Proxy NoArgsSignal) m
                return $ Right ()
            _             -> return $ Left TBadActionCtor),
        defSignal "noArgsSignal" (Proxy :: Proxy NoArgsSignal), 
        defMethod "fireInt" $ \m -> (expectActionRef m $ \a -> case a of
            ST1FireInt v -> do
                fireSignal (Proxy :: Proxy IntSignal) m v
                return $ Right ()
            _            -> return $ Left TBadActionCtor),
        defSignal "intSignal" (Proxy :: Proxy IntSignal),
        defMethod "fireThreeInts" $ \m -> (expectActionRef m $ \a -> case a of
            ST1FireThreeInts v1 v2 v3 -> do
                fireSignal (Proxy :: Proxy ThreeIntsSignal) m v1 v2 v3
                return $ Right ()
            _                         -> return $ Left TBadActionCtor),
        defSignal "threeIntsSignal" (Proxy :: Proxy ThreeIntsSignal),
        defMethod "fireDouble" $ \m -> (expectActionRef m $ \a -> case a of
            ST1FireDouble v -> do
                fireSignal (Proxy :: Proxy DoubleSignal) m v
                return $ Right ()
            _               -> return $ Left TBadActionCtor),
        defSignal "doubleSignal" (Proxy :: Proxy DoubleSignal),
        defMethod "fireText" $ \m -> (expectActionRef m $ \a -> case a of
            ST1FireText v -> do
                fireSignal (Proxy :: Proxy TextSignal) m v
                return $ Right ()
            _             -> return $ Left TBadActionCtor),
        defSignal "textSignal" (Proxy :: Proxy TextSignal),
        defMethod "fireObject" $ \m -> (expectActionRef m $ \a -> case a of
            ST1FireObject _ -> do
                (Right obj) <- getTestObject $ fromObjRef m
                fireSignal (Proxy :: Proxy ObjectSignal) m obj
                return $ Right ()
            _               -> return $ Left TBadActionCtor),
        defSignal "objectSignal" (Proxy :: Proxy ObjectSignal),
        defMethod "checkObject" $ \m v -> expectAction m $ \a -> case a of
            ST1CheckObject w -> setTestObject m v w
            _                -> return $ Left TBadActionCtor]
