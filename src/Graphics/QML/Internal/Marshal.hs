{-# LANGUAGE
    ScopedTypeVariables
  #-}

module Graphics.QML.Internal.Marshal where

import Data.Tagged
import Foreign.Ptr

-- | Represents a QML type name.
newtype TypeName = TypeName {
  typeName :: String
}

-- | The class 'MarshalIn' allows QML values to be converted into Haskell
-- values.
class MarshalIn a where
  mIn :: InMarshaller a

-- | Encapsulates the functionality to needed to implement an instance of
-- 'MarshalIn' so that such instances can be defined without access to
-- implementation details.
data InMarshaller a = InMarshaller {
  mInFuncFld :: Ptr () -> IO a,
  mIOTypeFld :: Tagged a TypeName
}

mInFunc :: (MarshalIn a) => Ptr () -> IO a
mInFunc = mInFuncFld mIn 

mIOType :: (MarshalIn a) => Tagged a TypeName
mIOType = mIOTypeFld mIn

-- | The class 'MarshalOut' allows Haskell values to be converted into QML
-- values.
class (MarshalIn a) => MarshalOut a where
  mOutFunc :: Ptr () -> a -> IO ()
  mOutSize :: Tagged a Int
