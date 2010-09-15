{-# LANGUAGE
    TypeSynonymInstances
  #-}

-- | Intrinsic types which can marshalled between Haskell and QML.
module Graphics.QML.Types.Intrinsics (
  Marshallable,
) where

import Graphics.QML.Internal.Core
import Graphics.QML.Internal.Intrinsics

import Data.Maybe
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Network.URI (URI, parseURIReference, uriToString)

--
-- ()/void built-in type
--

instance Marshallable () where
  marshal _ _ =
    error "Cannot marshal void."
  unmarshal _ =
    return ()
  mSizeOf _ = 0
  mTypeOf _ = TypeName ""

--
-- Int/int built-in type
--

instance Marshallable Int where
  marshal ptr value =
    poke (castPtr ptr :: Ptr CInt) (fromIntegral value)
  unmarshal ptr =
    peek (castPtr ptr :: Ptr CInt) >>= return . fromIntegral
  mSizeOf _ = sizeOf (0 :: CInt)
  mTypeOf _ = TypeName "int"

--
-- String/QString built-in type
--

instance Marshallable String where
  marshal ptr str =
    hsqmlMarshalString str (HsQMLStringHandle $ castPtr ptr)
  unmarshal ptr =
    hsqmlUnmarshalString (HsQMLStringHandle $ castPtr ptr)
  mSizeOf _ = hsqmlStringSize
  mTypeOf _ = TypeName "QString"

--
-- URI/QUrl built-in type
--

instance Marshallable URI where
  marshal ptr uri =
    hsqmlMarshalUrl (uriToString id uri "") (HsQMLUrlHandle $ castPtr ptr)
  unmarshal ptr =
    hsqmlUnmarshalUrl (HsQMLUrlHandle $ castPtr ptr) >>=
      return . fromJust . parseURIReference
  mSizeOf _ = hsqmlUrlSize
  mTypeOf _ = TypeName "QUrl"
