{-# LANGUAGE
    ScopedTypeVariables,
    TypeFamilies,
    FlexibleContexts,
    FlexibleInstances,
    Rank2Types
  #-}

module Graphics.QML.Internal.Objects where

import Graphics.QML.Internal.BindObj
import Graphics.QML.Internal.Marshal

import Data.Typeable
import Data.Typeable.Internal
import Data.Bits
import Data.Char

data MemberKind
    = MethodMember
    | PropertyMember
    | SignalMember
    deriving (Bounded, Enum, Eq)

-- | Represents a named member of the QML class which wraps type @tt@.
data Member tt = Member {
    memberKind   :: MemberKind,
    memberName   :: String,
    memberInit   :: Int -> IO (),
    memberType   :: TypeName,
    memberParams :: [TypeName],
    memberFun    :: UniformFunc,
    memberFunAux :: Maybe UniformFunc
}

-- | Represents the API of the QML class which wraps the type @tt@.
newtype ClassDef tt = ClassDef {
    classMembers :: [Member tt]
}

-- | The class 'Object' allows Haskell types to expose an object-oriented
-- interface to QML. 
class (Typeable tt) => Object tt where
  classDef :: ClassDef tt

type MObjToHsFunc t = HsQMLObjectHandle -> IO t
type MHsToObjFunc t = t -> IO HsQMLObjectHandle

-- | Type function yielding the object type specified by a given 'MarshalMode'.
type family ModeObj m

-- | Type function yielding the object type speficied by a given marshallable
-- type @tt@.
type ThisObj tt = ModeObj (MarshalMode tt)

-- | Class for 'MarshalMode's which support marshalling QML-to-Haskell
-- in contexts specific to objects.
class (MarshalBase m, Object (ModeObj m)) => MarshalFromObj m where
  mObjToHs_ :: forall t. Marshaller t m -> MObjToHsFunc t

mObjToHs ::
  forall t. (Marshal t, MarshalFromObj (MarshalMode t)) => MObjToHsFunc t
mObjToHs = mObjToHs_ (marshaller :: Marshaller t (MarshalMode t))

-- | Class for 'MarshalMode's which support marshalling Haskell-to-QML
-- in contexts specific to objects.
class (MarshalBase m, Object (ModeObj m)) => MarshalToObj m where
  mHsToObj_ :: forall t. Marshaller t m -> MHsToObjFunc t

mHsToObj ::
  forall t. (Marshal t, MarshalToObj (MarshalMode t)) => MHsToObjFunc t
mHsToObj = mHsToObj_ (marshaller :: Marshaller t (MarshalMode t))

-- | 'MarshalMode' for object types.
data ValObjBidi a
type instance ModeObj (ValObjBidi a) = a

data instance Marshaller t (ValObjBidi a) = MValObjBidi {
  mValObjBidi_typeName  :: !(MTypeNameFunc t),
  mValObjBidi_typeInit  :: !(MTypeInitFunc t),
  mValObjBidi_valToHs   :: !(MValToHsFunc t),
  mValObjBidi_hsToVal   :: !(MHsToValFunc t),
  mValObjBidi_hsToAlloc :: !(MHsToAllocFunc t),
  mValObjBidi_objToHs   :: !(MObjToHsFunc t),
  mValObjBidi_hsToObj   :: !(MHsToObjFunc t)}

instance MarshalBase (ValObjBidi a) where
  mTypeName_ = mValObjBidi_typeName
  mTypeInit_ = mValObjBidi_typeInit

instance MarshalToHs (ValObjBidi a) where
  mValToHs_ = mValObjBidi_valToHs

instance MarshalToValRaw (ValObjBidi a) where
  mHsToVal_   = mValObjBidi_hsToVal
  mHsToAlloc_ = mValObjBidi_hsToAlloc

instance MarshalToVal (ValObjBidi a) where

instance (Object a) => MarshalToObj (ValObjBidi a) where
  mHsToObj_ = mValObjBidi_hsToObj

instance (Object a) => MarshalFromObj (ValObjBidi a) where
  mObjToHs_ = mValObjBidi_objToHs

-- | 'MarshalMode' for object types, operating only in the QML-to-Haskell
-- direction.
data ValObjToOnly a

type instance ModeObj (ValObjToOnly a) = a

data instance Marshaller t (ValObjToOnly a) = MValObjToOnly {
  mValObjToOnly_typeName  :: !(MTypeNameFunc t),
  mValObjToOnly_typeInit  :: !(MTypeInitFunc t),
  mValObjToOnly_valToHs   :: !(MValToHsFunc t),
  mValObjToOnly_objToHs   :: !(MObjToHsFunc t)}

instance MarshalBase (ValObjToOnly a) where
  mTypeName_ = mValObjToOnly_typeName
  mTypeInit_ = mValObjToOnly_typeInit

instance MarshalToHs (ValObjToOnly a) where
  mValToHs_ = mValObjToOnly_valToHs

instance (Object a) => MarshalFromObj (ValObjToOnly a) where
  mObjToHs_ = mValObjToOnly_objToHs

