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

import Data.Char
import Data.Maybe
import Data.Tagged
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Network.URI (
    URI (URI), URIAuth (URIAuth),
    parseURIReference, unEscapeString,
    uriToString, escapeURIString, nullURI)

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
-- Double/double built-in type
--

instance MarshalOut Double where
  mOutFunc ptr num =
    poke (castPtr ptr :: Ptr CDouble) (realToFrac num)
  mOutSize = Tagged $ sizeOf (0 :: CDouble)

instance MarshalIn Double where
  mIn = InMarshaller {
    mInFuncFld = \ptr ->
      peek (castPtr ptr :: Ptr CDouble) >>= return . realToFrac,
    mIOTypeFld = Tagged $ TypeName "double"
  }

--
-- Text/QString built-in type
--

instance MarshalOut Text where
  mOutFunc ptr txt = do
    array <- hsqmlMarshalString
        (T.lengthWord16 txt) (HsQMLStringHandle $ castPtr ptr)
    T.unsafeCopyToPtr txt (castPtr array)
  mOutSize = Tagged hsqmlStringSize

instance MarshalIn Text where
  mIn = InMarshaller {
    mInFuncFld = \ptr -> do
      pair <- alloca (\bufPtr -> do
        len <- hsqmlUnmarshalString (HsQMLStringHandle $ castPtr ptr) bufPtr
        buf <- peek bufPtr
        return (castPtr buf, fromIntegral len))
      uncurry T.fromPtr pair,
    mIOTypeFld = Tagged $ TypeName "QString"
  }

--
-- String/QString built-in type
--

instance MarshalOut String where
  mOutFunc ptr str = mOutFunc ptr $ T.pack str
  mOutSize = Tagged hsqmlStringSize

instance MarshalIn String where
  mIn = InMarshaller {
    mInFuncFld = fmap T.unpack . mInFuncFld mIn,
    mIOTypeFld = Tagged $ TypeName "QString"
  }

--
-- URI/QUrl built-in type
--

mapURIStrings :: (String -> String) -> URI -> URI
mapURIStrings f (URI scheme auth path query frag) =
    URI (f scheme) (mapAuth auth) (f path) (f query) (f frag)
    where mapAuth (Just (URIAuth user name port)) =
              Just $ URIAuth (f user) (f name) (f port)
          mapAuth Nothing = Nothing

instance MarshalOut URI where
  mOutFunc ptr uri =
    let str = uriToString id (mapURIStrings (escapeURIString isLatin1) uri) ""
    in withCStringLen str (\(buf, bufLen) ->
         hsqmlMarshalUrl buf bufLen (HsQMLUrlHandle $ castPtr ptr))
  mOutSize = Tagged hsqmlUrlSize

instance MarshalIn URI where
  mIn = InMarshaller {
    mInFuncFld = \ptr -> do
      pair <- alloca (\bufPtr -> do
        len <- hsqmlUnmarshalUrl (HsQMLUrlHandle $ castPtr ptr) bufPtr
        buf <- peek bufPtr
        return (castPtr buf, fromIntegral len))
      str <- peekCStringLen pair
      free $ fst pair
      return $ mapURIStrings unEscapeString $
        fromMaybe nullURI $ parseURIReference str,
    mIOTypeFld = Tagged $ TypeName "QUrl"
  }
