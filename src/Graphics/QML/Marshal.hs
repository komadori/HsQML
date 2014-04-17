{-# LANGUAGE
    ScopedTypeVariables,
    TypeFamilies,
    TypeSynonymInstances,
    FlexibleInstances
  #-}

-- | Type classs and instances for marshalling values between Haskell and QML.
module Graphics.QML.Marshal (
  Marshal (
    type MarshalMode,
    marshaller),
  ModeBidi,
  ModeFrom,
  ModeRetVoid,
  ModeObjBidi,
  ModeObjFrom,
  ThisObj,
  Yes,
  CanGetFrom,
  CanGetFrom_,
  CanPassTo,
  CanPassTo_,
  CanReturnTo,
  CanReturnTo_,
  CanGetObjFrom,
  CanGetObjFrom_,
  CanPassObjTo,
  CanPassObjTo_,
  Marshaller 
) where

import Graphics.QML.Internal.BindPrim
import Graphics.QML.Internal.Marshal
import Graphics.QML.Internal.Objects

import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Tagged
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

--
-- Boolean built-in type
--

instance Marshal Bool where
    type MarshalMode Bool = ModeBidi
    marshaller = Marshaller {
        mTypeCVal_ = Tagged tyJSValue,
        mFromCVal_ = jvalFromCVal,
        mToCVal_ = jvalToCVal,
        mWithCVal_ = jvalWithCVal,
        mFromJVal_ = \ptr ->
            MaybeT $ fromJVal hsqmlIsJvalBool hsqmlGetJvalBool ptr,
        mWithJVal_ = \bool f ->
            withJVal hsqmlInitJvalBool bool f,
        mFromHndl_ = unimplFromHndl,
        mToHndl_ = unimplToHndl}

--
-- Int32/int built-in type
--

instance Marshal Int32 where
    type MarshalMode Int32 = ModeBidi
    marshaller = Marshaller {
        mTypeCVal_ = Tagged tyInt,
        mFromCVal_ = \ptr ->
            errIO $ peek (castPtr ptr :: Ptr CInt) >>= return . fromIntegral,
        mToCVal_ = \int ptr ->
            poke (castPtr ptr :: Ptr CInt) (fromIntegral int),
        mWithCVal_ = \int f ->
            alloca $ \(ptr :: Ptr CInt) ->
                mToCVal int (castPtr ptr) >> f (castPtr ptr),
        mFromJVal_ = \ptr ->
            MaybeT $ fromJVal hsqmlIsJvalNumber (
                fmap fromIntegral . hsqmlGetJvalInt) ptr,
        mWithJVal_ = \int f ->
            withJVal hsqmlInitJvalInt (fromIntegral int) f,
        mFromHndl_ = unimplFromHndl,
        mToHndl_ = unimplToHndl}

instance Marshal Int where
    type MarshalMode Int = ModeBidi
    marshaller = Marshaller {
        mTypeCVal_ = Tagged tyInt,
        mFromCVal_ = fmap (fromIntegral :: Int32 -> Int) . mFromCVal,
        mToCVal_ = \int ptr -> mToCVal (fromIntegral int :: Int32) ptr,
        mWithCVal_ = \int f -> mWithCVal (fromIntegral int :: Int32) f,
        mFromJVal_ = fmap (fromIntegral :: Int32 -> Int) . mFromJVal,
        mWithJVal_ = \int f -> mWithJVal (fromIntegral int :: Int32) f,
        mFromHndl_ = unimplFromHndl,
        mToHndl_ = unimplToHndl}

--
-- Double/double built-in type
--

instance Marshal Double where
    type MarshalMode Double = ModeBidi
    marshaller = Marshaller {
        mTypeCVal_ = Tagged tyDouble,
        mFromCVal_ = \ptr ->
            errIO $ peek (castPtr ptr :: Ptr CDouble) >>= return . realToFrac,
        mToCVal_ = \num ptr ->
            poke (castPtr ptr :: Ptr CDouble) (realToFrac num),
        mWithCVal_ = \num f ->
            alloca $ \(ptr :: Ptr CDouble) ->
                mToCVal num (castPtr ptr) >> f (castPtr ptr),
        mFromJVal_ = \ptr ->
            MaybeT $ fromJVal hsqmlIsJvalNumber (
                fmap realToFrac . hsqmlGetJvalDouble) ptr,
        mWithJVal_ = \num f ->
            withJVal hsqmlInitJvalDouble (realToFrac num) f,
        mFromHndl_ = unimplFromHndl,
        mToHndl_ = unimplToHndl}

--
-- Text/QString built-in type
--

instance Marshal Text where
    type MarshalMode Text = ModeBidi
    marshaller = Marshaller {
        mTypeCVal_ = Tagged tyString,
        mFromCVal_ = \ptr -> errIO $ do
            pair <- alloca (\bufPtr -> do
                len <- hsqmlReadString (
                    HsQMLStringHandle $ castPtr ptr) bufPtr
                buf <- peek bufPtr
                return (castPtr buf, fromIntegral len))
            uncurry T.fromPtr pair,
        mToCVal_ = \txt ptr -> do
            array <- hsqmlWriteString
                (T.lengthWord16 txt) (HsQMLStringHandle $ castPtr ptr)
            T.unsafeCopyToPtr txt (castPtr array),
        mWithCVal_ = \txt f ->
            withStrHndl $ \(HsQMLStringHandle ptr) -> do
                mToCVal txt $ castPtr ptr
                f $ castPtr ptr,
        mFromJVal_ = \jval ->
            MaybeT $ withStrHndl $ \sHndl -> runMaybeT $ do
                MaybeT $ fromJVal hsqmlIsJvalString (
                    flip hsqmlGetJvalString sHndl) jval
                let (HsQMLStringHandle ptr) = sHndl
                mFromCVal $ castPtr ptr,
        mWithJVal_ = \txt f ->
            mWithCVal txt $ \ptr -> withJVal hsqmlInitJvalString (
                HsQMLStringHandle $ castPtr ptr) f,
        mFromHndl_ = unimplFromHndl,
        mToHndl_ = unimplToHndl}

--
-- String/QString built-in type
--

instance Marshal String where
    type MarshalMode String = ModeBidi
    marshaller = Marshaller {
        mTypeCVal_ = Tagged tyString,
        mFromCVal_ = fmap T.unpack . mFromCVal,
        mToCVal_ = \txt ptr -> mToCVal (T.pack txt) ptr,
        mWithCVal_ = \txt f -> mWithCVal (T.pack txt) f,
        mFromJVal_ = fmap T.unpack . mFromJVal,
        mWithJVal_ = \txt f -> mWithJVal (T.pack txt) f,
        mFromHndl_ = unimplFromHndl,
        mToHndl_ = unimplToHndl}

--
-- Maybe
--

instance (Marshal a) => Marshal (Maybe a) where
    type MarshalMode (Maybe a) = ModeBidi
    marshaller = Marshaller {
        mTypeCVal_ = Tagged tyJSValue,
        mFromCVal_ = jvalFromCVal,
        mToCVal_ = jvalToCVal,
        mWithCVal_ = jvalWithCVal,
        mFromJVal_ = \jval -> errIO $ runMaybeT $ mFromJVal jval,
        mWithJVal_ = \val f ->
            case val of
                Just val' -> mWithJVal val' f
                Nothing   -> withJVal hsqmlInitJvalNull False f,
        mFromHndl_ = unimplFromHndl,
        mToHndl_ = unimplToHndl}
