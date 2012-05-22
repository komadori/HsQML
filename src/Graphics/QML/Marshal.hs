{-# LANGUAGE
    TypeSynonymInstances
  #-}

-- | Intrinsic types which can marshalled between Haskell and QML.
module Graphics.QML.Marshal (
  Marshallable,
) where

import Graphics.QML.Internal.Core
import Graphics.QML.Internal.Intrinsics

import Data.Maybe
import Data.Tagged
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
  mSizeOf = Tagged 0
  mTypeOf = Tagged $ TypeName ""

--
-- Int/int built-in type
--

instance Marshallable Int where
  marshal ptr value =
    poke (castPtr ptr :: Ptr CInt) (fromIntegral value)
  unmarshal ptr =
    peek (castPtr ptr :: Ptr CInt) >>= return . fromIntegral
  mSizeOf = Tagged $ sizeOf (0 :: CInt)
  mTypeOf = Tagged $ TypeName "int"

--
-- String/QString built-in type
--

instance Marshallable String where
  marshal ptr str =
    hsqmlMarshalString str (HsQMLStringHandle $ castPtr ptr)
  unmarshal ptr =
    hsqmlUnmarshalString (HsQMLStringHandle $ castPtr ptr)
  mSizeOf = Tagged $ hsqmlStringSize
  mTypeOf = Tagged $ TypeName "QString"

--
-- URI/QUrl built-in type
--

instance Marshallable URI where
  marshal ptr uri =
    hsqmlMarshalUrl (uriToString id uri "") (HsQMLUrlHandle $ castPtr ptr)
  unmarshal ptr =
    hsqmlUnmarshalUrl (HsQMLUrlHandle $ castPtr ptr) >>=
      return . fromJust . parseURIReference
  mSizeOf = Tagged $ hsqmlUrlSize
  mTypeOf = Tagged $ TypeName "QUrl"
