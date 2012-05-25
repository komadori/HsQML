{-# LANGUAGE
    ScopedTypeVariables
  #-}
{-# OPTIONS_HADDOCK hide #-}

module Graphics.QML.Internal.Core where

import Data.Tagged
import Foreign.Ptr
import Foreign.Marshal.Alloc

newtype TypeName = TypeName {typeName :: String}

-- | The class 'Marshallable' allows Haskell types to be used from QML.
class Marshallable a where
  marshal   :: Ptr () -> a -> IO ()
  unmarshal :: Ptr () -> IO a
  mSizeOf   :: Tagged a Int
  mTypeOf   :: Tagged a TypeName
