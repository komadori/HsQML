{-# LANGUAGE
    ScopedTypeVariables,
    TypeFamilies,
    Rank2Types
  #-}

module Graphics.QML.Internal.Marshal where

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

newtype TypeId = TypeId Int deriving (Eq, Ord)

tyInt, tyDouble, tyString, tyObject, tyVoid :: TypeId
tyInt     = TypeId 2
tyDouble  = TypeId 6
tyString  = TypeId 10
tyObject  = TypeId 39
tyVoid    = TypeId 43

type MTypeCValFunc t = Tagged t TypeId
type MFromCValFunc t = Ptr () -> ErrIO t
type MToCValFunc t = t -> Ptr () -> IO ()
type MWithCValFunc t = (forall b. t -> (Ptr () -> IO b) -> IO b)

type MFromHndlFunc t = HsQMLObjectHandle -> IO t
type MToHndlFunc t = t -> IO HsQMLObjectHandle

-- | The class 'Marshal' allows Haskell values to be marshalled to and from the
-- QML environment.
class Marshal t where
    -- | The 'MarshalMode' associated type parameter specifies the type of
    -- marshalling functionality offered by the instance.
    type MarshalMode t
    -- | Yields the 'Marshaller' for the type @t@.
    marshaller :: Marshaller t (MarshalMode t)

-- | 'MarshalMode' for non-object types with bidirectional marshalling.
data ModeBidi
type instance CanGetFrom_ ModeBidi = Yes
type instance CanPassTo_ ModeBidi = Yes
type instance CanReturnTo_ ModeBidi = Yes

-- | 'MarshalMode' for non-object types with from-only marshalling.
data ModeFrom
type instance CanGetFrom_ ModeFrom = Yes

-- | 'MarshalMode' for void in method returns.
data ModeRetVoid
type instance CanReturnTo_ ModeRetVoid = Yes

-- | 'MarshalMode' for object types with bidirectional marshalling.
data ModeObjBidi a
type instance CanGetFrom_ (ModeObjBidi a) = Yes
type instance CanPassTo_ (ModeObjBidi a) = Yes
type instance CanReturnTo_ (ModeObjBidi a) = Yes
type instance CanGetObjFrom_ (ModeObjBidi a) = Yes
type instance CanPassObjTo_ (ModeObjBidi a) = Yes
type instance ThisObj_ (ModeObjBidi a) = a

-- | 'MarshalMode' for object types with from-only marshalling.
data ModeObjFrom a
type instance CanGetFrom_ (ModeObjFrom a) = Yes
type instance CanGetObjFrom_ (ModeObjFrom a) = Yes
type instance ThisObj_ (ModeObjFrom a) = a

-- | Successful return value from marshalling capability type functions.
data Yes

-- | Type function equal to 'Yes' if the 'MarshalMode' @m@ supports receiving
-- values from QML.
type family CanGetFrom_ m

-- | Type function equal to 'Yes' if the marshallable type @t@ supports being
-- received from QML.
type CanGetFrom t = CanGetFrom_ (MarshalMode t)

-- | Type function equal to 'Yes' if the 'MarshalMode' @m@ supports passing
-- values to QML.
type family CanPassTo_ m

-- | Type function equal to 'Yes' if the marshallable type @t@ supports being
-- passed to QML.
type CanPassTo t = CanPassTo_ (MarshalMode t)

-- | Type function equal to 'Yes' if the 'MarshalMode' @m@ supports returning
-- values to QML.
type family CanReturnTo_ m

-- | Type function equal to 'Yes' if the marshallable type @t@ supports being
-- returned to QML.
type CanReturnTo t = CanReturnTo_ (MarshalMode t)

-- | Type function equal to 'Yes' if the 'MarshalMode' @m@ supports receiving
-- values from QML in the form of an object handle.
type family CanGetObjFrom_ m

-- | Type function equal to 'Yes' if the marshallable type @t@ supports being
-- received from QML in the form of an object handle.
type CanGetObjFrom t = CanGetObjFrom_ (MarshalMode t)

-- | Type function equal to 'Yes' if the 'MarshalMode' @m@ supports passing
-- values to QML in the form of an object handle.
type family CanPassObjTo_ m

-- | Type function equal to 'Yes' if the marshallable type @t@ supports being
-- passed to QML in the form of an object handle.
type CanPassObjTo t = CanPassObjTo_ (MarshalMode t)

-- | Type function yielding the object type specified by the 'MarshalMode' @m@.
type family ThisObj_ m

-- | Type function yielding the object type specified by the marshallable type
-- @t@.
type ThisObj t = ThisObj_ (MarshalMode t)

-- | Encapsulates the functionality to needed to implement an instance of
-- 'Marshal' so that such instances can be defined without access to
-- implementation details.
data Marshaller t m = Marshaller {
    mTypeCVal_ :: !(MTypeCValFunc t),
    mFromCVal_ :: !(MFromCValFunc t),
    mToCVal_   :: !(MToCValFunc t),
    mWithCVal_ :: !(MWithCValFunc t),
    mFromHndl_ :: !(MFromHndlFunc t),
    mToHndl_   :: !(MToHndlFunc t)
}

mTypeCVal :: forall t. (Marshal t) => MTypeCValFunc t
mTypeCVal = mTypeCVal_ (marshaller :: Marshaller t (MarshalMode t))

mFromCVal :: forall t. (Marshal t) => MFromCValFunc t
mFromCVal = mFromCVal_ (marshaller :: Marshaller t (MarshalMode t))

mToCVal :: forall t. (Marshal t) => MToCValFunc t
mToCVal = mToCVal_ (marshaller :: Marshaller t (MarshalMode t))

mWithCVal :: forall t. (Marshal t) => MWithCValFunc t
mWithCVal = mWithCVal_ (marshaller :: Marshaller t (MarshalMode t))

mFromHndl :: forall t. (Marshal t) => MFromHndlFunc t
mFromHndl = mFromHndl_ (marshaller :: Marshaller t (MarshalMode t))

mToHndl :: forall t. (Marshal t) => MToHndlFunc t
mToHndl = mToHndl_ (marshaller :: Marshaller t (MarshalMode t))

unimplFromCVal :: MFromCValFunc t
unimplFromCVal = \_ -> error "Type does not support mFromCVal."

unimplToCVal :: MToCValFunc t
unimplToCVal = \_ _ -> error "Type does not support mToCVal."

unimplWithCVal :: MWithCValFunc t
unimplWithCVal = \_ _ -> error "Type does not support mWithCVal."

unimplFromHndl :: MFromHndlFunc t
unimplFromHndl = \_ -> error "Type does not support mFromHndl."

unimplToHndl :: MToHndlFunc t
unimplToHndl = \_ -> error "Type does not support mToHndl."

instance Marshal () where
    type MarshalMode () = ModeRetVoid
    marshaller = Marshaller {
        mTypeCVal_ = Tagged tyVoid,
        mFromCVal_ = unimplFromCVal,
        mToCVal_ = \_ _ -> return (),
        mWithCVal_ = unimplWithCVal,
        mFromHndl_ = unimplFromHndl,
        mToHndl_ = unimplToHndl}

