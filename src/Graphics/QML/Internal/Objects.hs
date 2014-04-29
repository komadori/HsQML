module Graphics.QML.Internal.Objects where

import Graphics.QML.Internal.BindObj
import Graphics.QML.Internal.Marshal

import Data.Typeable
import Data.Unique

data MemberKind
    = MethodMember
    | PropertyMember
    | SignalMember
    deriving (Bounded, Enum, Eq)

data MemberKey
    = TypeKey TypeRep
    | DataKey Unique
    deriving (Eq, Ord)

-- | Represents a named member of the QML class which wraps type @tt@.
data Member tt = Member {
    memberKind   :: MemberKind,
    memberName   :: String,
    memberType   :: TypeId,
    memberParams :: [TypeId],
    memberFun    :: UniformFunc,
    memberFunAux :: Maybe UniformFunc,
    memberKey    :: Maybe MemberKey
}

-- | Represents the API of the QML class which wraps the type @tt@.
newtype ClassDef tt = ClassDef {
    classMembers :: [Member tt]
}

-- | The class 'Object' allows Haskell types to expose an object-oriented
-- interface to QML. 
class (Typeable tt) => Object tt where
  classDef :: ClassDef tt
