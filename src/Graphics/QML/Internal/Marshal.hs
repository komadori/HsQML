{-# LANGUAGE
    ScopedTypeVariables,
    TypeFamilies,
    FlexibleContexts,
    FlexibleInstances,
    Rank2Types
  #-}

module Graphics.QML.Internal.Marshal where

import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Tagged
import Foreign.Ptr
import System.IO

-- | Represents a QML type name.
newtype TypeName = TypeName {
  typeName :: String
}

type ErrIO a = MaybeT IO a

runErrIO :: ErrIO a -> IO ()
runErrIO m = do
  r <- runMaybeT m
  if isNothing r
  then hPutStrLn stderr "Warning: Marshalling error."
  else return ()

errIO :: IO a -> ErrIO a
errIO = MaybeT . fmap Just

type MTypeNameFunc t = Tagged t TypeName
type MValToHsFunc t = Ptr () -> ErrIO t
type MHsToValFunc t = t -> Ptr () -> IO ()
type MHsToAllocFunc t = (forall b. t -> (Ptr () -> IO b) -> IO b)

-- | The class 'Marshal' allows Haskell values to be marshalled to and from the
-- QML environment.
class Marshal t where
  -- | The 'MarshalMode' associated type parameter specifies the type of
  -- marshalling functionality offered by the instance.
  type MarshalMode t
  -- | Yields the 'Marshaller' for the type @t@.
  marshaller :: Marshaller t (MarshalMode t)

-- | Base class containing core functionality for all 'MarshalMode's.
class MarshalBase m where
  mTypeName_ :: forall t. Marshaller t m -> MTypeNameFunc t

mTypeName ::
  forall t. (Marshal t, MarshalBase (MarshalMode t)) => MTypeNameFunc t
mTypeName = mTypeName_ (marshaller :: Marshaller t (MarshalMode t))

-- | Class for 'MarshalMode's which support marshalling QML-to-Haskell.
class (MarshalBase m) => MarshalToHs m where
  mValToHs_ :: forall t. Marshaller t m -> MValToHsFunc t

mValToHs ::
  forall t. (Marshal t, MarshalToHs (MarshalMode t)) => MValToHsFunc t
mValToHs = mValToHs_ (marshaller :: Marshaller t (MarshalMode t))

-- | Class for 'MarshalMode's which support marshalling Haskell-to-QML.
class (MarshalBase m) => MarshalToValRaw m where
  mHsToVal_   :: forall t. Marshaller t m -> MHsToValFunc t
  mHsToAlloc_ :: forall t. Marshaller t m -> MHsToAllocFunc t

mHsToVal ::
  forall t. (Marshal t, MarshalToValRaw (MarshalMode t)) => MHsToValFunc t
mHsToVal = mHsToVal_ (marshaller :: Marshaller t (MarshalMode t))

mHsToAlloc ::
  forall t. (Marshal t, MarshalToValRaw (MarshalMode t)) => MHsToAllocFunc t
mHsToAlloc = mHsToAlloc_ (marshaller :: Marshaller t (MarshalMode t))

-- | Class for 'MarshalMode's which support marshalling Haskell-to-QML,
-- excluding the return of void from methods.
class (MarshalToValRaw m) => MarshalToVal m where

-- | Encapsulates the functionality to needed to implement an instance of
-- 'Marshal' so that such instances can be defined without access to
-- implementation details.
data family Marshaller t m

-- | 'MarshalMode' for built-in data types.
data ValBidi

data instance Marshaller t ValBidi = MValBidi {
  mValBidi_typeName  :: !(MTypeNameFunc t),
  mValBidi_valToHs   :: !(MValToHsFunc t),
  mValBidi_hsToVal   :: !(MHsToValFunc t),
  mValBidi_hsToAlloc :: !(MHsToAllocFunc t)}

instance MarshalBase ValBidi where
  mTypeName_ = mValBidi_typeName

instance MarshalToHs ValBidi where
  mValToHs_ = mValBidi_valToHs

instance MarshalToValRaw ValBidi where
  mHsToVal_   = mValBidi_hsToVal
  mHsToAlloc_ = mValBidi_hsToAlloc

instance MarshalToVal ValBidi where

-- | 'MarshalMode' for void in method returns.
data ValFnRetVoid

data instance Marshaller t ValFnRetVoid = MValFnRetVoid {
  mValFnRetVoid_typeName  :: !(MTypeNameFunc t),
  mValFnRetVoid_hsToVal   :: !(MHsToValFunc t),
  mValFnRetVoid_hsToAlloc :: !(MHsToAllocFunc t)}

instance MarshalBase ValFnRetVoid where
  mTypeName_ = mValFnRetVoid_typeName

instance MarshalToValRaw ValFnRetVoid where
  mHsToVal_   = mValFnRetVoid_hsToVal
  mHsToAlloc_ = mValFnRetVoid_hsToAlloc

instance Marshal () where
  type MarshalMode () = ValFnRetVoid
  marshaller = MValFnRetVoid {
    mValFnRetVoid_typeName = Tagged $ TypeName "",
    mValFnRetVoid_hsToVal = \_ _ -> return (),
    mValFnRetVoid_hsToAlloc = \_ f -> f nullPtr}
