{-# LANGUAGE
    ScopedTypeVariables,
    TypeFamilies,
    FlexibleInstances
  #-}

-- | Type classs and instances for marshalling values between Haskell and QML.
module Graphics.QML.Marshal (
  -- * Marshalling Type-class
  Marshal (
    type MarshalMode,
    marshaller),
  ModeBidi,
  ModeFrom,
  ModeTo,
  ModeRetVoid,
  ModeObjBidi,
  ModeObjFrom,
  ModeObjTo,
  Yes,
  CanGetFrom,
  ICanGetFrom,
  CanPassTo,
  ICanPassTo,
  CanReturnTo,
  ICanReturnTo,
  IsObjType,
  IIsObjType,
  GetObjType,
  IGetObjType,
  Marshaller,

  -- * Data Types
  Ignored (
    Ignored),

  -- * Custom Marshallers
  bidiMarshallerIO,
  bidiMarshaller,
  fromMarshallerIO,
  fromMarshaller,
  toMarshallerIO,
  toMarshaller
) where

import Graphics.QML.Internal.BindPrim
import Graphics.QML.Internal.Marshal
import Graphics.QML.Internal.Types

import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Tagged
import Data.Int
import Data.Text (Text)
import qualified Data.Text.Foreign as T
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

--
-- Boolean built-in type
--

instance Marshal Bool where
    type MarshalMode Bool c d = ModeBidi c
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
    type MarshalMode Int32 c d = ModeBidi c
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
    type MarshalMode Int c d = ModeBidi c
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
    type MarshalMode Double c d = ModeBidi c
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
    type MarshalMode Text c d = ModeBidi c
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
-- Maybe
--

instance (Marshal a) => Marshal (Maybe a) where
    type MarshalMode (Maybe a) ICanGetFrom d = MarshalMode a ICanGetFrom d
    type MarshalMode (Maybe a) ICanPassTo d = MarshalMode a ICanPassTo d
    type MarshalMode (Maybe a) ICanReturnTo d = MarshalMode a ICanReturnTo d
    type MarshalMode (Maybe a) IIsObjType d = No
    type MarshalMode (Maybe a) IGetObjType d = No
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

--
-- List
--

instance (Marshal a) => Marshal [a] where
    type MarshalMode [a] ICanGetFrom d = MarshalMode a ICanGetFrom d
    type MarshalMode [a] ICanPassTo d = MarshalMode a ICanPassTo d
    type MarshalMode [a] ICanReturnTo d = MarshalMode a ICanReturnTo d
    type MarshalMode [a] IIsObjType d = No
    type MarshalMode [a] IGetObjType d = No
    marshaller = Marshaller {
        mTypeCVal_ = Tagged tyJSValue,
        mFromCVal_ = jvalFromCVal,
        mToCVal_ = jvalToCVal,
        mWithCVal_ = jvalWithCVal,
        mFromJVal_ = \jval -> MaybeT $ do
            len <- hsqmlGetJvalArrayLength jval
            withJVal hsqmlInitJvalNull True $ \tmp ->
                runMaybeT $ forM [0..len-1] $ \i -> do
                    errIO $ hsqmlJvalArrayGet jval i tmp
                    mFromJVal tmp,
        mWithJVal_ = \vs f ->
            withJVal hsqmlInitJvalArray (length vs) $ \jval -> do
                forM_ (zip [0..] vs) $ uncurry $ \i val ->
                    mWithJVal val $ \jval' ->
                        hsqmlJvalArraySet jval i jval'
                f jval,
        mFromHndl_ = unimplFromHndl,
        mToHndl_ = unimplToHndl}

--
-- Ignored
--

-- | Represents an argument whose value is ignored.

data Ignored = Ignored

instance Marshal Ignored where
    type MarshalMode Ignored c d = ModeFrom c
    marshaller = Marshaller {
        mTypeCVal_ = Tagged tyJSValue,
        mFromCVal_ = jvalFromCVal,
        mToCVal_ = unimplToCVal,
        mWithCVal_ = unimplWithCVal,
        mFromJVal_ = \_ -> MaybeT . return $ Just Ignored,
        mWithJVal_ = unimplWithJVal,
        mFromHndl_ = unimplFromHndl,
        mToHndl_ = unimplToHndl}

type BidiMarshaller a b = Marshaller b
    (MarshalMode a ICanGetFrom ())
    (MarshalMode a ICanPassTo ())
    (MarshalMode a ICanReturnTo ())
    (MarshalMode a IIsObjType ())
    (MarshalMode a IGetObjType ())

-- | Provides a bidirectional 'Marshaller' which allows you to define an
-- instance of 'Marshal' for your own type @b@ in terms of another marshallable
-- type @a@. Type @b@ should have a 'MarshalMode' of 'ModeObjBidi' or
-- 'ModeBidi' depending on whether @a@ was an object type or not.
bidiMarshallerIO ::
    forall a b. (Marshal a, CanGetFrom a ~ Yes, CanPassTo a ~ Yes) =>
    (a -> IO b) -> (b -> IO a) -> BidiMarshaller a b
bidiMarshallerIO fromFn toFn = Marshaller {
    mTypeCVal_ = retag (mTypeCVal :: Tagged a TypeId),
    mFromCVal_ = \ptr -> (errIO . fromFn) =<< mFromCVal ptr,
    mToCVal_ = \val ptr -> flip mToCVal ptr =<< toFn val,
    mWithCVal_ = \val f -> flip mWithCVal f =<< toFn val,
    mFromJVal_ = \ptr -> (errIO . fromFn) =<< mFromJVal ptr,
    mWithJVal_ = \val f -> flip mWithJVal f =<< toFn val,
    mFromHndl_ = \hndl -> fromFn =<< mFromHndl hndl,
    mToHndl_ = \val -> mToHndl =<< toFn val}

-- | Variant of 'bidiMarshallerIO' where the conversion functions between types
-- @a@ and @b@ do not live in the IO monad.
bidiMarshaller ::
    forall a b. (Marshal a, CanGetFrom a ~ Yes, CanPassTo a ~ Yes) =>
    (a -> b) -> (b -> a) -> BidiMarshaller a b
bidiMarshaller fromFn toFn =
    bidiMarshallerIO (return . fromFn) (return . toFn)

type FromMarshaller a b = Marshaller b
    (MarshalMode a ICanGetFrom ())
    No
    No
    (MarshalMode a IIsObjType ())
    (MarshalMode a IGetObjType ())

-- | Provides a "from" 'Marshaller' which allows you to define an instance of
-- 'Marshal' for your own type @b@ in terms of another marshallable type @a@.
-- Type @b@ should have a 'MarshalMode' of 'ModeObjFrom' or 'ModeFrom'
-- depending on whether @a@ was an object type or not.
fromMarshallerIO ::
    forall a b. (Marshal a, CanGetFrom a ~ Yes) =>
    (a -> IO b) -> FromMarshaller a b
fromMarshallerIO fromFn = Marshaller {
    mTypeCVal_ = retag (mTypeCVal :: Tagged a TypeId),
    mFromCVal_ = \ptr -> (errIO . fromFn) =<< mFromCVal ptr,
    mToCVal_ = unimplToCVal,
    mWithCVal_ = unimplWithCVal,
    mFromJVal_ = \ptr -> (errIO . fromFn) =<< mFromJVal ptr,
    mWithJVal_ = unimplWithJVal,
    mFromHndl_ = \hndl -> fromFn =<< mFromHndl hndl,
    mToHndl_ = unimplToHndl}

-- | Variant of 'fromMarshallerIO' where the conversion function between types
-- @a@ and @b@ does not live in the IO monad.
fromMarshaller ::
    forall a b. (Marshal a, CanGetFrom a ~ Yes) =>
    (a -> b) -> FromMarshaller a b
fromMarshaller fromFn = fromMarshallerIO (return . fromFn)

type ToMarshaller a b = Marshaller b
    No
    (MarshalMode a ICanPassTo ())
    (MarshalMode a ICanReturnTo ())
    (MarshalMode a IIsObjType ())
    (MarshalMode a IGetObjType ())

-- | Provides a "to" 'Marshaller' which allows you to define an instance of
-- 'Marshal' for your own type @b@ in terms of another marshallable type @a@.
-- Type @b@ should have a 'MarshalMode' of 'ModeObjTo' or 'ModeTo'
-- depending on whether @a@ was an object type or not.
toMarshallerIO ::
    forall a b. (Marshal a, CanPassTo a ~ Yes) =>
    (b -> IO a) -> ToMarshaller a b
toMarshallerIO toFn = Marshaller {
    mTypeCVal_ = retag (mTypeCVal :: Tagged a TypeId),
    mFromCVal_ = unimplFromCVal,
    mToCVal_ = \val ptr -> flip mToCVal ptr =<< toFn val,
    mWithCVal_ = \val f -> flip mWithCVal f =<< toFn val,
    mFromJVal_ = unimplFromJVal,
    mWithJVal_ = \val f -> flip mWithJVal f =<< toFn val,
    mFromHndl_ = unimplFromHndl,
    mToHndl_ = \val -> mToHndl =<< toFn val}

-- | Variant of 'toMarshallerIO' where the conversion function between types
-- @a@ and @b@ does not live in the IO monad.
toMarshaller ::
    forall a b. (Marshal a, CanPassTo a ~ Yes) =>
    (b -> a) -> ToMarshaller a b
toMarshaller toFn = toMarshallerIO (return . toFn)
