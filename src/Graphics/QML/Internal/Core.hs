{-# LANGUAGE
    ScopedTypeVariables
  #-}
{-# OPTIONS_HADDOCK hide #-}

module Graphics.QML.Internal.Core where

import Foreign.Ptr
import Foreign.Marshal.Alloc

newtype TypeName = TypeName {typeName :: String}

-- | The class 'Marshallable' allows Haskell types to be used from QML.
class Marshallable a where
  marshal   :: Ptr () -> a -> IO ()
  unmarshal :: Ptr () -> IO a
  mSizeOf   :: a -> Int
  mTypeOf   :: a -> TypeName

withMarshal :: (Marshallable a) => a -> (Ptr b -> IO c) -> IO c
withMarshal m f =
  allocaBytes (mSizeOf m) (\ptr -> marshal (castPtr ptr) m >> f ptr)
  
