{-# LANGUAGE
    ScopedTypeVariables,
    TypeFamilies,
    Rank2Types
  #-}

module Graphics.QML.Internal.Marshal where

import Graphics.QML.Internal.Types
import Graphics.QML.Internal.BindPrim
import Graphics.QML.Internal.BindObj

import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Tagged
import Foreign.Ptr
import System.IO

type ErrIO a = MaybeT IO a

runErrIO :: ErrIO a -> IO ()
runErrIO m = do
  r <- runMaybeT m
  if isNothing r
  then hPutStrLn stderr "Warning: Marshalling error."
  else return ()

errIO :: IO a -> ErrIO a
errIO = MaybeT . fmap Just

tyInt, tyDouble, tyString, tyObject, tyVoid, tyJSValue :: TypeId
tyInt     = TypeId 2
tyDouble  = TypeId 6
tyString  = TypeId 10
tyObject  = TypeId 39
tyVoid    = TypeId 43
tyJSValue = TypeId $ hsqmlJValTypeId

type MTypeCValFunc t = Tagged t TypeId
type MFromCValFunc t = Ptr () -> ErrIO t
type MToCValFunc t = t -> Ptr () -> IO ()
type MWithCValFunc t = (forall b. t -> (Ptr () -> IO b) -> IO b)

type MFromJValFunc t = HsQMLJValHandle -> ErrIO t
type MWithJValFunc t = (forall b. t -> (HsQMLJValHandle -> IO b) -> IO b)

type MFromHndlFunc t = HsQMLObjectHandle -> IO t
type MToHndlFunc t = t -> IO HsQMLObjectHandle

type MarshallerFor t = Marshaller t
    (MarshalMode t ICanGetFrom ()) (MarshalMode t ICanPassTo ())
    (MarshalMode t ICanReturnTo ())
    (MarshalMode t IIsObjType ()) (MarshalMode t IGetObjType ())

type MarshallerForMode t m = Marshaller t
    (m ICanGetFrom) (m ICanPassTo) (m ICanReturnTo)
    (m IIsObjType) (m IGetObjType)

-- | The class 'Marshal' allows Haskell values to be marshalled to and from the
-- QML environment.
class Marshal t where
    -- | The 'MarshalMode' associated type family specifies the marshalling
    -- capabilities offered by the instance. @c@ indicates the capability being
    -- queried. @d@ is dummy parameter which allows certain instances to type
    -- check.
    type MarshalMode t c d
    -- | Yields the 'Marshaller' for the type @t@.
    marshaller :: MarshallerFor t

-- | 'MarshalMode' for non-object types with bidirectional marshalling.
type family ModeBidi c
type instance ModeBidi ICanGetFrom = Yes
type instance ModeBidi ICanPassTo = Yes
type instance ModeBidi ICanReturnTo = Yes
type instance ModeBidi IIsObjType = No
type instance ModeBidi IGetObjType = No

-- | 'MarshalMode' for non-object types with from-only marshalling.
type family ModeFrom c
type instance ModeFrom ICanGetFrom = Yes
type instance ModeFrom ICanPassTo = No
type instance ModeFrom ICanReturnTo = No
type instance ModeFrom IIsObjType = No
type instance ModeFrom IGetObjType = No

-- | 'MarshalMode' for non-object types with to-only marshalling.
type family ModeTo c
type instance ModeTo ICanGetFrom = No
type instance ModeTo ICanPassTo = Yes
type instance ModeTo ICanReturnTo = Yes
type instance ModeTo IIsObjType = No
type instance ModeTo IGetObjType = No

-- | 'MarshalMode' for void in method returns.
type family ModeRetVoid c
type instance ModeRetVoid ICanGetFrom = No
type instance ModeRetVoid ICanPassTo = No
type instance ModeRetVoid ICanReturnTo = Yes
type instance ModeRetVoid IIsObjType = No
type instance ModeRetVoid IGetObjType = No

-- | 'MarshalMode' for object types with bidirectional marshalling.
type family ModeObjBidi a c
type instance ModeObjBidi a ICanGetFrom = Yes
type instance ModeObjBidi a ICanPassTo = Yes
type instance ModeObjBidi a ICanReturnTo = Yes
type instance ModeObjBidi a IIsObjType = Yes
type instance ModeObjBidi a IGetObjType = a

-- | 'MarshalMode' for object types with from-only marshalling.
type family ModeObjFrom a c
type instance ModeObjFrom a ICanGetFrom = Yes
type instance ModeObjFrom a ICanPassTo = No
type instance ModeObjFrom a ICanReturnTo = No
type instance ModeObjFrom a IIsObjType = Yes
type instance ModeObjFrom a IGetObjType = a

-- | 'MarshalMode' for object types with to-only marshalling.
type family ModeObjTo a c
type instance ModeObjTo a ICanGetFrom = No
type instance ModeObjTo a ICanPassTo = Yes
type instance ModeObjTo a ICanReturnTo = Yes
type instance ModeObjTo a IIsObjType = Yes
type instance ModeObjTo a IGetObjType = a

-- | Type value indicating a capability is supported.
data Yes

-- | Type value indicating a capability is not supported.
data No

-- | Type function equal to 'Yes' if the marshallable type @t@ supports being
-- received from QML.
type CanGetFrom t = MarshalMode t ICanGetFrom ()

-- | Type index into 'MarshalMode' for querying if the mode supports receiving
-- values from QML.
data ICanGetFrom

-- | Type function equal to 'Yes' if the marshallable type @t@ supports being
-- passed to QML.
type CanPassTo t = MarshalMode t ICanPassTo ()

-- | Type index into 'MarshalMode' for querying if the mode supports passing
-- values to QML.
data ICanPassTo

-- | Type function equal to 'Yes' if the marshallable type @t@ supports being
-- returned to QML.
type CanReturnTo t = MarshalMode t ICanReturnTo ()

-- | Type index into 'MarshalMode' for querying if the mode supports returning
-- values to QML.
data ICanReturnTo

-- | Type function equal to 'Yes' if the marshallable type @t@ is an object.
type IsObjType t = MarshalMode t IIsObjType ()

-- | Type index into 'MarshalMode' for querying if the mode supports an object
-- type.
data IIsObjType

-- | Type function which returns the type encapsulated by the object handles
-- used by the marshallable type @t@.
type GetObjType t = MarshalMode t IGetObjType ()

-- | Type index into 'MarshalMode' for querying the type encapsulated by the
-- mode's object handles.
data IGetObjType

-- | Encapsulates the functionality to needed to implement an instance of
-- 'Marshal' so that such instances can be defined without access to
-- implementation details.
data Marshaller t u v w x y = Marshaller {
    mTypeCVal_ :: !(MTypeCValFunc t),
    mFromCVal_ :: !(MFromCValFunc t),
    mToCVal_   :: !(MToCValFunc t),
    mWithCVal_ :: !(MWithCValFunc t),
    mFromJVal_ :: !(MFromJValFunc t),
    mWithJVal_ :: !(MWithJValFunc t),
    mFromHndl_ :: !(MFromHndlFunc t),
    mToHndl_   :: !(MToHndlFunc t)
}

mTypeCVal :: forall t. (Marshal t) => MTypeCValFunc t
mTypeCVal = mTypeCVal_ (marshaller :: MarshallerFor t)

mFromCVal :: forall t. (Marshal t) => MFromCValFunc t
mFromCVal = mFromCVal_ (marshaller :: MarshallerFor t)

mToCVal :: forall t. (Marshal t) => MToCValFunc t
mToCVal = mToCVal_ (marshaller :: MarshallerFor t)

mWithCVal :: forall t. (Marshal t) => MWithCValFunc t
mWithCVal = mWithCVal_ (marshaller :: MarshallerFor t)

mFromJVal :: forall t. (Marshal t) => MFromJValFunc t
mFromJVal = mFromJVal_ (marshaller :: MarshallerFor t)

mWithJVal :: forall t. (Marshal t) => MWithJValFunc t
mWithJVal = mWithJVal_ (marshaller :: MarshallerFor t)

mFromHndl :: forall t. (Marshal t) => MFromHndlFunc t
mFromHndl = mFromHndl_ (marshaller :: MarshallerFor t)

mToHndl :: forall t. (Marshal t) => MToHndlFunc t
mToHndl = mToHndl_ (marshaller :: MarshallerFor t)

unimplFromCVal :: MFromCValFunc t
unimplFromCVal = \_ -> error "Type does not support mFromCVal."

unimplToCVal :: MToCValFunc t
unimplToCVal = \_ _ -> error "Type does not support mToCVal."

unimplWithCVal :: MWithCValFunc t
unimplWithCVal = \_ _ -> error "Type does not support mWithCVal."

unimplFromJVal :: MFromJValFunc t
unimplFromJVal = \_ -> error "Type does not support mFromJVal."

unimplWithJVal :: MWithJValFunc t
unimplWithJVal = \_ _ -> error "Type does not support mWithJVal."

unimplFromHndl :: MFromHndlFunc t
unimplFromHndl = \_ -> error "Type does not support mFromHndl."

unimplToHndl :: MToHndlFunc t
unimplToHndl = \_ -> error "Type does not support mToHndl."

jvalFromCVal :: (Marshal t) => MFromCValFunc t
jvalFromCVal = mFromJVal . HsQMLJValHandle . castPtr

jvalToCVal :: (Marshal t) => MToCValFunc t
jvalToCVal = \val ptr -> mWithJVal val $ \jval ->
    hsqmlSetJval (HsQMLJValHandle $ castPtr ptr) jval

jvalWithCVal :: (Marshal t) => MWithCValFunc t
jvalWithCVal = \val f -> mWithJVal val $ \(HsQMLJValHandle ptr) ->
    f $ castPtr ptr

instance Marshal () where
    type MarshalMode () c d = ModeRetVoid c
    marshaller = Marshaller {
        mTypeCVal_ = Tagged tyVoid,
        mFromCVal_ = unimplFromCVal,
        mToCVal_ = \_ _ -> return (),
        mWithCVal_ = unimplWithCVal,
        mFromJVal_ = unimplFromJVal,
        mWithJVal_ = unimplWithJVal,
        mFromHndl_ = unimplFromHndl,
        mToHndl_ = unimplToHndl}
