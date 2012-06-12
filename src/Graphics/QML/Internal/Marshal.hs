{-# LANGUAGE
    ScopedTypeVariables
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

-- | The class 'MarshalIn' allows QML values to be converted into Haskell
-- values.
class MarshalIn a where
  mIn :: InMarshaller a

type ErrIO a = MaybeT IO a

runErrIO :: ErrIO a -> IO ()
runErrIO m = do
  r <- runMaybeT m
  if isNothing r
  then hPutStrLn stderr "Warning: Marshalling error."
  else return ()

errIO :: IO a -> ErrIO a
errIO = MaybeT . fmap Just

-- | Encapsulates the functionality to needed to implement an instance of
-- 'MarshalIn' so that such instances can be defined without access to
-- implementation details.
data InMarshaller a = InMarshaller {
  mInFuncFld :: Ptr () -> ErrIO a,
  mIOTypeFld :: Tagged a TypeName
}

mInFunc :: (MarshalIn a) => Ptr () -> ErrIO a
mInFunc = mInFuncFld mIn 

mIOType :: (MarshalIn a) => Tagged a TypeName
mIOType = mIOTypeFld mIn

-- | The class 'MarshalOut' allows Haskell values to be converted into QML
-- values.
class (MarshalIn a) => MarshalOut a where
  mOutFunc :: Ptr () -> a -> IO ()
  mOutSize :: Tagged a Int

instance MarshalOut () where
  mOutFunc _ _ = return ()
  mOutSize     = Tagged 0

instance MarshalIn () where
  mIn = InMarshaller {
    mInFuncFld = \_ -> return (),
    mIOTypeFld = Tagged $ TypeName ""
  }
