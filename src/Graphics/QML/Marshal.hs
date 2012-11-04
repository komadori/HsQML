{-# LANGUAGE
    ScopedTypeVariables,
    TypeSynonymInstances,
    FlexibleInstances
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
    uriToString, escapeURIString, nullURI,
    isUnescapedInURI)

--
-- Int/int built-in type
--

instance MarshalOut Int where
  mOutFunc ptr int =
    poke (castPtr ptr :: Ptr CInt) (fromIntegral int)
  mOutAlloc num f =
    alloca $ \(ptr :: Ptr CInt) ->
      mOutFunc (castPtr ptr) num >> f (castPtr ptr)

instance MarshalIn Int where
  mIn = InMarshaller {
    mInFuncFld = \ptr ->
      errIO $ peek (castPtr ptr :: Ptr CInt) >>= return . fromIntegral,
    mIOTypeFld = Tagged $ TypeName "int",
    mIOInitFld = Tagged $ return ()
  }

--
-- Double/double built-in type
--

instance MarshalOut Double where
  mOutFunc ptr num =
    poke (castPtr ptr :: Ptr CDouble) (realToFrac num)
  mOutAlloc num f =
    alloca $ \(ptr :: Ptr CDouble) ->
      mOutFunc (castPtr ptr) num >> f (castPtr ptr)

instance MarshalIn Double where
  mIn = InMarshaller {
    mInFuncFld = \ptr ->
      errIO $ peek (castPtr ptr :: Ptr CDouble) >>= return . realToFrac,
    mIOTypeFld = Tagged $ TypeName "double",
    mIOInitFld = Tagged $ return ()
  }

--
-- Text/QString built-in type
--

instance MarshalOut Text where
  mOutFunc ptr txt = do
    array <- hsqmlMarshalString
        (T.lengthWord16 txt) (HsQMLStringHandle $ castPtr ptr)
    T.unsafeCopyToPtr txt (castPtr array)
  mOutAlloc txt f =
    allocaBytes hsqmlStringSize $ \ptr -> do
      hsqmlInitString $ HsQMLStringHandle ptr
      mOutFunc (castPtr ptr) txt
      ret <- f (castPtr ptr)
      hsqmlDeinitString $ HsQMLStringHandle ptr
      return ret

instance MarshalIn Text where
  mIn = InMarshaller {
    mInFuncFld = \ptr -> errIO $ do
      pair <- alloca (\bufPtr -> do
        len <- hsqmlUnmarshalString (HsQMLStringHandle $ castPtr ptr) bufPtr
        buf <- peek bufPtr
        return (castPtr buf, fromIntegral len))
      uncurry T.fromPtr pair,
    mIOTypeFld = Tagged $ TypeName "QString",
    mIOInitFld = Tagged $ return ()
  }

--
-- String/QString built-in type
--

instance MarshalOut String where
  mOutFunc ptr str = mOutFunc ptr $ T.pack str
  mOutAlloc txt f =
    allocaBytes hsqmlStringSize $ \ptr -> do
      hsqmlInitString $ HsQMLStringHandle ptr
      mOutFunc (castPtr ptr) txt
      ret <- f (castPtr ptr)
      hsqmlDeinitString $ HsQMLStringHandle ptr
      return ret

instance MarshalIn String where
  mIn = InMarshaller {
    mInFuncFld = fmap T.unpack . mInFuncFld mIn,
    mIOTypeFld = Tagged $ TypeName "QString",
    mIOInitFld = Tagged $ return ()
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
    let str = uriToString id (mapURIStrings
                (escapeURIString isUnescapedInURI) uri) ""
    in withCStringLen str (\(buf, bufLen) ->
         hsqmlMarshalUrl buf bufLen (HsQMLUrlHandle $ castPtr ptr))
  mOutAlloc uri f =
    allocaBytes hsqmlUrlSize $ \ptr -> do
      hsqmlInitUrl $ HsQMLUrlHandle ptr
      mOutFunc (castPtr ptr) uri
      ret <- f (castPtr ptr)
      hsqmlDeinitUrl $ HsQMLUrlHandle ptr
      return ret

instance MarshalIn URI where
  mIn = InMarshaller {
    mInFuncFld = \ptr -> errIO $ do
      pair <- alloca (\bufPtr -> do
        len <- hsqmlUnmarshalUrl (HsQMLUrlHandle $ castPtr ptr) bufPtr
        buf <- peek bufPtr
        return (castPtr buf, fromIntegral len))
      str <- peekCStringLen pair
      free $ fst pair
      return $ mapURIStrings unEscapeString $
        fromMaybe nullURI $ parseURIReference str,
    mIOTypeFld = Tagged $ TypeName "QUrl",
    mIOInitFld = Tagged $ return ()
  }
