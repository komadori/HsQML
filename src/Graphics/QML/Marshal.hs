{-# LANGUAGE
    TypeSynonymInstances
  #-}

-- | Type classs and instances for marshalling values between Haskell and QML.
module Graphics.QML.Marshal (
  MarshalIn (
    mIn),
  InMarshaller,
  MarshalOut
) where

import Graphics.QML.Internal.Marshal
import Graphics.QML.Internal.PrimValues

import Data.Maybe
import Data.Tagged
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Network.URI (URI, parseURIReference, uriToString)

--
-- ()/void built-in type
--

instance MarshalOut () where
  mOutFunc _ _ = return ()
  mOutSize     = Tagged 0

instance MarshalIn () where
  mIn = InMarshaller {
    mInFuncFld = \_ -> return (),
    mIOTypeFld = Tagged $ TypeName ""
  }

--
-- Int/int built-in type
--

instance MarshalOut Int where
  mOutFunc ptr int =
    poke (castPtr ptr :: Ptr CInt) (fromIntegral int)
  mOutSize = Tagged $ sizeOf (0 :: CInt)

instance MarshalIn Int where
  mIn = InMarshaller {
    mInFuncFld = \ptr ->
      peek (castPtr ptr :: Ptr CInt) >>= return . fromIntegral,
    mIOTypeFld = Tagged $ TypeName "int"
  }

--
-- String/QString built-in type
--

instance MarshalOut String where
  mOutFunc ptr str =
    hsqmlMarshalString str (HsQMLStringHandle $ castPtr ptr)
  mOutSize = Tagged hsqmlStringSize

instance MarshalIn String where
  mIn = InMarshaller {
    mInFuncFld = \ptr ->
      hsqmlUnmarshalString (HsQMLStringHandle $ castPtr ptr),
    mIOTypeFld = Tagged $ TypeName "QString"
  }

--
-- URI/QUrl built-in type
--

instance MarshalOut URI where
  mOutFunc ptr uri =
    hsqmlMarshalUrl (uriToString id uri "") (HsQMLUrlHandle $ castPtr ptr)
  mOutSize = Tagged hsqmlUrlSize

instance MarshalIn URI where
  mIn = InMarshaller {
    mInFuncFld = \ptr ->
      hsqmlUnmarshalUrl (HsQMLUrlHandle $ castPtr ptr) >>=
        return . fromJust . parseURIReference,
    mIOTypeFld = Tagged $ TypeName "QUrl"
  }
