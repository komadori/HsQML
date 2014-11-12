module Graphics.QML.Internal.Types where

import Data.Map (Map)
import Data.Typeable
import Data.Unique
import Foreign.Ptr

newtype TypeId = TypeId Int deriving (Eq, Ord)

type UniformFunc = Ptr () -> Ptr (Ptr ()) -> IO ()

data MemberKey
    = TypeKey TypeRep
    | DataKey Unique
    deriving (Eq, Ord)

data ClassInfo = ClassInfo {
    cinfoObjType :: TypeRep,
    cinfoSignals :: Map MemberKey Int
}
