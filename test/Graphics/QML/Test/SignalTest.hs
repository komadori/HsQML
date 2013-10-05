{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, TypeFamilies #-}

module Graphics.QML.Test.SignalTest where

import Graphics.QML.Objects
import Graphics.QML.Test.Framework
import Graphics.QML.Test.MayGen
import Graphics.QML.Test.GenURI
import Graphics.QML.Test.ScriptDSL (Expr, Prog(Prog))
import qualified Graphics.QML.Test.ScriptDSL as S

import Test.QuickCheck.Arbitrary
import Control.Applicative
import Data.Monoid
import Data.Tagged
import Data.Typeable

import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI

data SignalTest1
    = ST1TrivialMethod
    | ST1FireNoArgs
    | ST1FireInt Int32
    | ST1FireDouble Double
    | ST1FireString String
    | ST1FireText Text
    | ST1FireURI URI
    deriving (Show, Typeable)

data NoArgsSignal deriving Typeable

instance SignalKey NoArgsSignal where
    type SignalParams NoArgsSignal = IO ()

data IntSignal deriving Typeable

instance SignalKey IntSignal where
    type SignalParams IntSignal = Int32 -> IO ()

data DoubleSignal deriving Typeable

instance SignalKey DoubleSignal where
    type SignalParams DoubleSignal = Double -> IO ()

data StringSignal deriving Typeable

instance SignalKey StringSignal where
    type SignalParams StringSignal = String -> IO ()

data TextSignal deriving Typeable

instance SignalKey TextSignal where
    type SignalParams TextSignal = Text -> IO ()

data URISignal deriving Typeable

instance SignalKey URISignal where
    type SignalParams URISignal = URI -> IO ()

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
    nextActionsFor _ = mayOneof [
        pure ST1TrivialMethod,
        pure ST1FireNoArgs,
        ST1FireInt <$> fromGen arbitrary,
        ST1FireDouble <$> fromGen arbitrary,
        ST1FireString <$> fromGen arbitrary,
        ST1FireText . T.pack <$> fromGen arbitrary,
        ST1FireURI <$> fromGen uriGen]
    updateEnvRaw _ e = e
    actionRemote ST1TrivialMethod n =
        S.eval $ (S.var n) `S.dot` "trivialMethod" `S.call` []
    actionRemote ST1FireNoArgs n =
        chainSignal n [] "noArgsSignal" "fireNoArgs"
    actionRemote (ST1FireInt v) n =
        testSignal n "intSignal" "fireInt" $ S.literal v
    actionRemote (ST1FireDouble v) n =
        testSignal n "doubleSignal" "fireDouble" $ S.literal v
    actionRemote (ST1FireString v) n =
        testSignal n "stringSignal" "fireString" $ S.literal v
    actionRemote (ST1FireText v) n =
        testSignal n "textSignal" "fireText" $ S.literal v
    actionRemote (ST1FireURI v) n =
        testSignal n "uriSignal" "fireURI" $ S.literal v
    mockObjDef = [
        defMethod "trivialMethod" $ \m -> expectAction m $ \a -> case a of
            ST1TrivialMethod -> return $ Right ()
            _                -> return $ Left TBadActionCtor,
        defMethod "fireNoArgs" $ \m -> (expectActionRef m $ \a -> case a of
            ST1FireNoArgs -> do
                fireSignal (Tagged m
                    :: Tagged NoArgsSignal (ObjRef (MockObj SignalTest1)))
                return $ Right ()
            _             -> return $ Left TBadActionCtor),
        defSignal (Tagged "noArgsSignal" :: Tagged NoArgsSignal String), 
        defMethod "fireInt" $ \m -> (expectActionRef m $ \a -> case a of
            ST1FireInt v -> do
                fireSignal (Tagged m
                    :: Tagged IntSignal (ObjRef (MockObj SignalTest1))) v
                return $ Right ()
            _            -> return $ Left TBadActionCtor),
        defSignal (Tagged "intSignal" :: Tagged IntSignal String),
        defMethod "fireDouble" $ \m -> (expectActionRef m $ \a -> case a of
            ST1FireDouble v -> do
                fireSignal (Tagged m
                    :: Tagged DoubleSignal (ObjRef (MockObj SignalTest1))) v
                return $ Right ()
            _               -> return $ Left TBadActionCtor),
        defSignal (Tagged "doubleSignal" :: Tagged DoubleSignal String),
        defMethod "fireString" $ \m -> (expectActionRef m $ \a -> case a of
            ST1FireString v -> do
                fireSignal (Tagged m
                    :: Tagged StringSignal (ObjRef (MockObj SignalTest1))) v
                return $ Right ()
            _               -> return $ Left TBadActionCtor),
        defSignal (Tagged "stringSignal" :: Tagged StringSignal String),
        defMethod "fireText" $ \m -> (expectActionRef m $ \a -> case a of
            ST1FireText v -> do
                fireSignal (Tagged m
                    :: Tagged TextSignal (ObjRef (MockObj SignalTest1))) v
                return $ Right ()
            _             -> return $ Left TBadActionCtor),
        defSignal (Tagged "textSignal" :: Tagged TextSignal String),
        defMethod "fireURI" $ \m -> (expectActionRef m $ \a -> case a of
            ST1FireURI v -> do
                fireSignal (Tagged m
                    :: Tagged URISignal (ObjRef (MockObj SignalTest1))) v
                return $ Right ()
            _            -> return $ Left TBadActionCtor),
        defSignal (Tagged "uriSignal" :: Tagged URISignal String)] 
