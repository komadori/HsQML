{-# LANGUAGE
    ScopedTypeVariables,
    TypeFamilies,
    TypeSynonymInstances,
    FlexibleInstances
  #-}

-- | Type classs and instances for marshalling values between Haskell and QML.
module Graphics.QML.Marshal (
  Marshal (
    type MarshalMode,
    marshaller),
  MarshalToHs,
  MarshalToValRaw,
  MarshalToVal,
  MarshalFromObj,
  MarshalToObj,
  ValBidi,
  ValFnRetVoid,
  ValObjBidi,
  ValObjToOnly,
  ThisObj,
  Marshaller 
) where

import Graphics.QML.Internal.BindPrim
import Graphics.QML.Internal.Marshal
import Graphics.QML.Internal.Objects

import Data.Maybe
import Data.Tagged
import Data.Int
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
-- Int32/int built-in type
--

instance Marshal Int32 where
  type MarshalMode Int32 = ValBidi
  marshaller = MValBidi {
    mValBidi_typeName = Tagged $ TypeName "int",
    mValBidi_valToHs = \ptr ->
      errIO $ peek (castPtr ptr :: Ptr CInt) >>= return . fromIntegral,
    mValBidi_hsToVal = \int ptr ->
      poke (castPtr ptr :: Ptr CInt) (fromIntegral int),
    mValBidi_hsToAlloc = \int f ->
      alloca $ \(ptr :: Ptr CInt) ->
        mHsToVal int (castPtr ptr) >> f (castPtr ptr)}

instance Marshal Int where
  type MarshalMode Int = ValBidi
  marshaller = MValBidi {
    mValBidi_typeName = Tagged $ TypeName "int",
    mValBidi_valToHs = fmap (fromIntegral :: Int32 -> Int) . mValToHs,
    mValBidi_hsToVal = \int ptr -> mHsToVal (fromIntegral int :: Int32) ptr,
    mValBidi_hsToAlloc = \int f -> mHsToAlloc (fromIntegral int :: Int32) f}

--
-- Double/double built-in type
--

instance Marshal Double where
  type MarshalMode Double = ValBidi
  marshaller = MValBidi {
    mValBidi_typeName = Tagged $ TypeName "double",
    mValBidi_valToHs = \ptr ->
      errIO $ peek (castPtr ptr :: Ptr CDouble) >>= return . realToFrac,
    mValBidi_hsToVal = \num ptr ->
      poke (castPtr ptr :: Ptr CDouble) (realToFrac num),
    mValBidi_hsToAlloc = \num f ->
      alloca $ \(ptr :: Ptr CDouble) ->
        mHsToVal num (castPtr ptr) >> f (castPtr ptr)}

--
-- Text/QString built-in type
--

instance Marshal Text where
  type MarshalMode Text = ValBidi
  marshaller = MValBidi {
    mValBidi_typeName = Tagged $ TypeName "QString",
    mValBidi_valToHs = \ptr -> errIO $ do
      pair <- alloca (\bufPtr -> do
        len <- hsqmlUnmarshalString (HsQMLStringHandle $ castPtr ptr) bufPtr
        buf <- peek bufPtr
        return (castPtr buf, fromIntegral len))
      uncurry T.fromPtr pair,
    mValBidi_hsToVal = \txt ptr -> do
      array <- hsqmlMarshalString
          (T.lengthWord16 txt) (HsQMLStringHandle $ castPtr ptr)
      T.unsafeCopyToPtr txt (castPtr array),
    mValBidi_hsToAlloc = \txt f ->
      allocaBytes hsqmlStringSize $ \ptr -> do
        hsqmlInitString $ HsQMLStringHandle ptr
        mHsToVal txt (castPtr ptr)
        ret <- f (castPtr ptr)
        hsqmlDeinitString $ HsQMLStringHandle ptr
        return ret}

--
-- String/QString built-in type
--

instance Marshal String where
  type MarshalMode String = ValBidi
  marshaller = MValBidi {
    mValBidi_typeName = Tagged $ TypeName "QString",
    mValBidi_valToHs = fmap T.unpack . mValToHs,
    mValBidi_hsToVal = \txt ptr -> mHsToVal (T.pack txt) ptr,
    mValBidi_hsToAlloc = \txt f -> mHsToAlloc (T.pack txt) f}

--
-- URI/QUrl built-in type
--

mapURIStrings :: (String -> String) -> URI -> URI
mapURIStrings f (URI scheme auth path query frag) =
    URI (f scheme) (mapAuth auth) (f path) (f query) (f frag)
    where mapAuth (Just (URIAuth user name port)) =
              Just $ URIAuth (f user) (f name) (f port)
          mapAuth Nothing = Nothing

instance Marshal URI where
  type MarshalMode URI = ValBidi
  marshaller = MValBidi {
    mValBidi_typeName = Tagged $ TypeName "QUrl",
    mValBidi_valToHs = \ptr -> errIO $ do
      pair <- alloca (\bufPtr -> do
        len <- hsqmlUnmarshalUrl (HsQMLUrlHandle $ castPtr ptr) bufPtr
        buf <- peek bufPtr
        return (castPtr buf, fromIntegral len))
      str <- peekCStringLen pair
      free $ fst pair
      return $ mapURIStrings unEscapeString $
        fromMaybe nullURI $ parseURIReference str,
    mValBidi_hsToVal = \uri ptr ->
      let str = uriToString id (mapURIStrings
                  (escapeURIString isUnescapedInURI) uri) ""
      in withCStringLen str (\(buf, bufLen) ->
           hsqmlMarshalUrl buf bufLen (HsQMLUrlHandle $ castPtr ptr)),
    mValBidi_hsToAlloc = \uri f ->
      allocaBytes hsqmlUrlSize $ \ptr -> do
        hsqmlInitUrl $ HsQMLUrlHandle ptr
        mHsToVal uri (castPtr ptr)
        ret <- f (castPtr ptr)
        hsqmlDeinitUrl $ HsQMLUrlHandle ptr
        return ret}
