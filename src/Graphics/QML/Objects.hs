{-# LANGUAGE
    ScopedTypeVariables,
    TypeFamilies,
    FlexibleContexts
  #-}

-- | Facilities for defining new object types which can be marshalled between
-- Haskell and QML.
module Graphics.QML.Objects (
  -- * Class Definition
  Object (
    classDef),
  ClassDef,
  Member,
  defClass,

  -- * Methods
  defMethod0,
  defMethod1,
  defMethod2,
  defMethod3,

  -- * Properties
  defPropertyRO,
  defPropertyRW,

  -- * Object References
  ObjRef,
  newObject,
  fromObjRef,

  -- * Marshalling Type-classes
  objectInMarshaller,
  MarshalThis (
    type ThisObj,
    mThis),
  objectThisMarshaller
) where

import Graphics.QML.Internal.Marshal
import Graphics.QML.Internal.Objects
import Graphics.QML.Internal.Engine

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Bits
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Tagged
import Data.Typeable
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import System.IO.Unsafe
import Numeric

--
-- ObjRef
--

-- | The class 'MarshalThis' allows objects to be marshalled back into
-- Haskell as the \"this\" value for callbacks.
class (Object (ThisObj tt)) => MarshalThis tt where
  type ThisObj tt
  mThis :: ThisMarshaller tt

-- | Encapsulates the functionality to needed to implement an instance of
-- 'MarshalThis' so that such instances can be defined without access to
-- implementation details.
data ThisMarshaller tt = ThisMarshaller {
  mThisFuncFld :: Ptr () -> IO tt
}

mThisFunc :: (MarshalThis tt) => Ptr () -> IO tt
mThisFunc = mThisFuncFld mThis

instance (Object tt) => MarshalOut (ObjRef tt) where
  mOutFunc ptr obj = do
    objPtr <- hsqmlObjectGetPointer $ objHndl obj
    poke (castPtr ptr) objPtr
  mOutSize = Tagged $ sizeOf nullPtr

instance (Object tt) => MarshalIn (ObjRef tt) where
  mIn = InMarshaller {
    mInFuncFld = \ptr -> MaybeT $ do
      objPtr <- peek (castPtr ptr)
      hndl <- hsqmlGetObjectHandle objPtr $
        Just $ classHndl (classDef :: ClassDef tt)
      return $ if isNullObjectHandle hndl
        then Nothing else Just $ ObjRef hndl,
    mIOTypeFld = Tagged $ TypeName "QObject*"
  }

instance (Object tt) => MarshalThis (ObjRef tt) where
  type ThisObj (ObjRef tt) = tt
  mThis = ThisMarshaller {
      mThisFuncFld = \ptr -> do
      hndl <- hsqmlGetObjectHandle ptr Nothing
      return $ ObjRef hndl
  }

retagType :: Tagged (ObjRef tt) TypeName -> Tagged tt TypeName
retagType = retag

-- | Provides an 'InMarshaller' which allows you to define instances of
-- 'MarshalIn' for custom object types. For example:
--
-- @
-- instance MarshalIn MyObjectType where
--     mIn = objectInMarshaller
-- @
--
-- This instance would allow @MyObjectType@ to be used as a parameter type
-- in callbacks. An instance is provided for @'ObjRef' MyObjectType@ by
-- default.
objectInMarshaller :: (Object tt) => InMarshaller tt
objectInMarshaller =
  InMarshaller {
    mInFuncFld = fmap fromObjRef . mInFunc,
    mIOTypeFld = retagType mIOType
  }

-- | Provides an 'ThisMarshaller' which allows you to define instances of
-- 'MarshalThis' for custom object types. For example:
--
-- @
-- instance MarshalThis MyObjectType where
--     type (ThisObj MyObjectType) = MyObjectType
--     mIn = objectInMarshaller
-- @
--
-- This instance would allow @MyObjectType@ to be used as the \"this\" type
-- for callbacks. An instance is provided for @'ObjRef' MyObjectType@ by
-- default.
objectThisMarshaller :: (Object tt, (ThisObj tt) ~ tt) => ThisMarshaller tt
objectThisMarshaller =
  ThisMarshaller {
    mThisFuncFld = fmap fromObjRef . mThisFunc
  }

-- | Creates an instance of a QML class given a value of the underlying Haskell 
-- type @tt@.
newObject :: forall tt. (Object tt) => tt -> IO (ObjRef tt)
newObject obj = do
  hndl <- hsqmlCreateObject obj $ classHndl (classDef :: ClassDef tt)
  return $ ObjRef hndl

-- | Returns the associated value of the underlying Haskell type @tt@ from an
-- instance of the QML class which wraps it.
fromObjRef :: ObjRef tt -> tt
fromObjRef =
    unsafePerformIO . hsqmlObjectGetHaskell . objHndl

--
-- Object
--

-- | The class 'Object' allows Haskell types to expose an object-oriented
-- interface to QML. 
{-# NOINLINE classDef #-}
class (Typeable tt) => Object tt where
  classDef :: ClassDef tt

--
-- ClassDef
--

-- | Represents the API of the QML class which wraps the type @tt@.
data ClassDef tt = ClassDef {
  classType :: TypeName,
  classHndl :: HsQMLClassHandle
}

-- | Generates a 'ClassDef' from a list of 'Member's.
defClass :: forall tt. (Object tt) => [Member tt] -> ClassDef tt
defClass ms = unsafePerformIO $ do
  let typ  = typeOf (undefined :: tt)
      name = tyConString $ typeRepTyCon typ
  id <- hsqmlGetNextClassId
  createClass (showString name $ showChar '_' $ showInt id "") ms

createClass :: forall tt. (Object tt) =>
  String -> [Member tt] -> IO (ClassDef tt)
createClass name ms = do
  let methods = methodMembers ms
      properties = propertyMembers ms
      (MOCOutput metaData metaStrData) = compileClass name methods properties
  metaDataPtr <- newArray metaData
  metaStrDataPtr <- newArray metaStrData
  methodsPtr <- mapM (marshalFunc . methodFunc) methods >>= newArray
  pReads <- mapM (marshalFunc . propertyReadFunc) properties
  pWrites <- mapM (fromMaybe (return nullFunPtr) . fmap marshalFunc .
    propertyWriteFunc) properties
  propertiesPtr <- newArray $ interleave pReads pWrites
  hsqmlInit
  hndl <- hsqmlCreateClass metaDataPtr metaStrDataPtr methodsPtr propertiesPtr
  return $ case hndl of 
    Just hndl' -> ClassDef (TypeName name) hndl'
    Nothing    -> error ("Failed to create QML class '"++name++"'.")

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave (x:xs) ys = x : ys `interleave` xs 

--
-- Member
--

-- | Represents a named member of the QML class which wraps type @tt@.
data Member tt
  -- | Constructs a 'Member' from a 'Method'.
  = MethodMember (Method tt)
  -- | Constructs a 'Member' from a 'Property'.
  | PropertyMember (Property tt)

-- | Returns the methods in a list of members.
methodMembers :: [Member tt] -> [Method tt]
methodMembers = mapMaybe f
  where f (MethodMember m) = Just m
        f _ = Nothing

-- | Returns the properties in a list of members.
propertyMembers :: [Member tt] -> [Property tt]
propertyMembers = mapMaybe f
  where f (PropertyMember m) = Just m
        f _ = Nothing

--
-- Method
--

-- | Represents a named method which can be invoked from QML on an object of
-- type @tt@.
data Method tt = Method {
  -- | Gets the name of a 'Method'.
  methodName  :: String,
  -- | Gets the 'TypeName's which comprise the signature of a 'Method'.
  -- The head of the list is the return type and the tail the arguments.
  methodTypes :: [TypeName],
  methodFunc  :: UniformFunc
}

-- | Defines a named method using an impure nullary function.
defMethod0 ::
  forall tt tr. (MarshalThis tt, MarshalOut tr) =>
  String -> (tt -> IO tr) -> Member (ThisObj tt)
defMethod0 name f = MethodMember $ Method name
  [untag (mIOType :: Tagged tr TypeName)]
  (marshalFunc0 $ \pt pr -> mThisFunc pt >>= f >>= marshalRet pr)

-- | Defines a named method using an impure unary function.
defMethod1 ::
  forall tt t1 tr. (MarshalThis tt, MarshalIn t1, MarshalOut tr) =>
  String -> (tt -> t1 -> IO tr) -> Member (ThisObj tt)
defMethod1 name f = MethodMember $ Method name
  [untag (mIOType :: Tagged tr TypeName),
   untag (mIOType :: Tagged t1 TypeName)]
  (marshalFunc1 $ \pt p1 pr -> runErrIO $ do
    vt <- errIO $ mThisFunc pt
    v1 <- mInFunc p1
    errIO $ f vt v1 >>= marshalRet pr)

-- | Defines a named method using an impure binary function.
defMethod2 ::
  forall tt t1 t2 tr.
  (MarshalThis tt, MarshalIn t1, MarshalIn t2, MarshalOut tr) =>
  String -> (tt -> t1 -> t2 -> IO tr) -> Member (ThisObj tt)
defMethod2 name f = MethodMember $ Method name
  [untag (mIOType :: Tagged tr TypeName),
   untag (mIOType :: Tagged t1 TypeName),
   untag (mIOType :: Tagged t2 TypeName)]
  (marshalFunc2 $ \pt p1 p2 pr -> runErrIO $ do
    vt <- errIO $ mThisFunc pt
    v1 <- mInFunc p1
    v2 <- mInFunc p2
    errIO $ f vt v1 v2 >>= marshalRet pr)

-- | Defines a named method using an impure function taking 3 arguments.
defMethod3 ::
  forall tt t1 t2 t3 tr.
  (MarshalThis tt, MarshalIn t1, MarshalIn t2, MarshalIn t3,
   MarshalOut tr) =>
  String -> (tt -> t1 -> t2 -> t3 -> IO tr) -> Member (ThisObj tt)
defMethod3 name f = MethodMember $ Method name
  [untag (mIOType :: Tagged tr TypeName),
   untag (mIOType :: Tagged t1 TypeName),
   untag (mIOType :: Tagged t2 TypeName),
   untag (mIOType :: Tagged t3 TypeName)]
  (marshalFunc3 $ \pt p1 p2 p3 pr -> runErrIO $ do
    vt <- errIO $ mThisFunc pt
    v1 <- mInFunc p1
    v2 <- mInFunc p2
    v3 <- mInFunc p3
    errIO $ f vt v1 v2 v3 >>= marshalRet pr)

--
-- Property
--

-- | Represents a named property which can be accessed from QML on an object
-- of type @tt@.
data Property tt = Property {
  -- | Gets the name of a 'Property'.
  propertyName :: String,
  propertyType :: TypeName,
  propertyReadFunc :: UniformFunc,
  propertyWriteFunc :: Maybe UniformFunc
}

-- | Defines a named read-only property using an impure
-- accessor function.
defPropertyRO ::
  forall tt tr. (MarshalThis tt, MarshalOut tr) =>
  String -> (tt -> IO tr) -> Member (ThisObj tt)
defPropertyRO name g = PropertyMember $ Property name
  (untag (mIOType :: Tagged tr TypeName))
  (marshalFunc0 $ \pt pr -> mThisFunc pt >>= g >>= mOutFunc pr)
  Nothing

-- | Defines a named read-write property using a pair of 
-- impure accessor and mutator functions.
defPropertyRW ::
  forall tt tr. (MarshalThis tt, MarshalOut tr) =>
  String -> (tt -> IO tr) -> (tt -> tr -> IO ()) -> Member (ThisObj tt)
defPropertyRW name g s = PropertyMember $ Property name
  (untag (mIOType :: Tagged tr TypeName))
  (marshalFunc0 $ \pt pr -> mThisFunc pt >>= g >>= mOutFunc pr)
  (Just $ marshalFunc1 $ \pt p1 _ -> runErrIO $ do
    vt <- errIO $ mThisFunc pt
    v1 <- mInFunc p1
    errIO $ s vt v1)

--
-- ???
--

marshalFunc0 :: (Ptr () -> Ptr () -> IO ()) -> UniformFunc
marshalFunc0 f pt pv = do
  pr <- peekElemOff pv 0
  f pt pr

marshalFunc1 :: (Ptr () -> Ptr () -> Ptr () -> IO ()) -> UniformFunc
marshalFunc1 f pt pv = do
  pr <- peekElemOff pv 0
  p1 <- peekElemOff pv 1
  f pt p1 pr

marshalFunc2 ::
  (Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO ()) -> UniformFunc
marshalFunc2 f pt pv = do
  pr <- peekElemOff pv 0
  p1 <- peekElemOff pv 1
  p2 <- peekElemOff pv 2
  f pt p1 p2 pr

marshalFunc3 ::
  (Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO ()) -> UniformFunc
marshalFunc3 f pt pv = do
  pr <- peekElemOff pv 0
  p1 <- peekElemOff pv 1
  p2 <- peekElemOff pv 2
  p3 <- peekElemOff pv 3
  f pt p1 p2 p3 pr

marshalRet :: (MarshalOut tt) => Ptr () -> tt -> IO ()
marshalRet ptr obj
  | ptr == nullPtr = return ()
  | otherwise      = mOutFunc ptr obj

data MOCState = MOCState {
  mData            :: [CUInt],
  mDataLen         :: Int,
  mDataMethodsIdx  :: Maybe Int,
  mDataPropsIdx    :: Maybe Int,
  mStrData         :: [CChar],
  mStrDataLen      :: Int,
  mStrDataMap      :: Map String CUInt
} deriving Show

data MOCOutput = MOCOutput [CUInt] [CChar]

newMOCState :: MOCState
newMOCState = MOCState [] 0 Nothing Nothing [] 0 Map.empty

writeInt :: CUInt -> State MOCState ()
writeInt int = do
  state <- get
  let md    = mData state
      mdLen = mDataLen state
  put $ state {mData = int:md, mDataLen = mdLen+1}
  return ()

writeString :: String -> State MOCState ()
writeString str = do
  state <- get
  let msd    = mStrData state
      msdLen = mStrDataLen state
      msdMap = mStrDataMap state
  case (Map.lookup str msdMap) of
    Just idx -> writeInt idx
    Nothing  -> do
      let idx = fromIntegral msdLen
          msd' = 0 : (map castCharToCChar (reverse str) ++ msd)
          msdLen' = msdLen + length str + 1
          msdMap' = Map.insert str idx msdMap
      put $ state {
        mStrData = msd',
        mStrDataLen = msdLen',
        mStrDataMap = msdMap'}
      writeInt idx

writeMethod :: Method tt -> State MOCState ()
writeMethod m = do
  idx <- get >>= return . mDataLen
  writeString $ methodSignature m
  writeString $ methodParameters m
  writeString $ typeName $ head $ methodTypes m
  writeString ""
  writeInt (mfAccessPublic .|. mfMethodScriptable)
  state <- get
  put $ state {mDataMethodsIdx = mplus (mDataMethodsIdx state) (Just idx)}
  return ()

writeProperty :: Property tt -> State MOCState ()
writeProperty p = do
  idx <- get >>= return . mDataLen
  writeString $ propertyName p
  writeString $ typeName $ propertyType p
  writeInt (pfReadable .|. pfScriptable .|.
    if (isJust $ propertyWriteFunc p) then pfWritable else 0)
  state <- get
  put $ state {mDataPropsIdx = mplus (mDataPropsIdx state) (Just idx)}
  return ()

compileClass :: String -> [Method tt] -> [Property tt] -> MOCOutput
compileClass name ms ps = 
  let enc = flip execState newMOCState $ do
        writeInt 5                           -- Revision
        writeString name                     -- Class name
        writeInt 0 >> writeInt 0             -- Class info
        writeInt $ fromIntegral $ length ms  -- Methods
        writeInt $ fromIntegral $
          fromMaybe 0 $ mDataMethodsIdx enc  -- Methods (data index)
        writeInt $ fromIntegral $ length ps  -- Properties
        writeInt $ fromIntegral $
          fromMaybe 0 $ mDataPropsIdx enc    -- Properties (data index)
        writeInt 0 >> writeInt 0             -- Enums
        writeInt 0 >> writeInt 0             -- Constructors
        writeInt 0                           -- Flags
        writeInt 0                           -- Signals        
        mapM_ writeMethod ms
        mapM_ writeProperty ps
        writeInt 0
  in MOCOutput (reverse $ mData enc) (reverse $ mStrData enc)

foldr0 :: (a -> a -> a) -> a -> [a] -> a
foldr0 _ x [] = x
foldr0 f _ xs = foldr1 f xs

methodSignature :: Method tt -> String
methodSignature method =
  let paramTypes = tail $ methodTypes method
  in (showString (methodName method) . showChar '(' .
       foldr0 (\l r -> l . showChar ',' . r) id
         (map (showString . typeName) paramTypes) . showChar ')') ""

methodParameters :: Method tt -> String
methodParameters method =
  replicate (flip (-) 2 $ length $ methodTypes method) ','
