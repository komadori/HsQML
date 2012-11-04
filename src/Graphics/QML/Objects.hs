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
  defMethod,
  MethodSuffix,

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
import Control.Exception
import Data.Bits
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Tagged
import Data.Typeable
import Data.IORef
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
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

addPointer :: TypeName -> TypeName
addPointer (TypeName name) = TypeName $ name ++ "*"

instance (Object tt) => MarshalOut (ObjRef tt) where
  mOutFunc ptr obj = do
    objPtr <- hsqmlObjectGetPointer $ objHndl obj
    poke (castPtr ptr) objPtr
  mOutAlloc obj f =
    alloca $ \(ptr :: Ptr (Ptr ())) ->
      mOutFunc (castPtr ptr) obj >> f (castPtr ptr)

instance (Object tt) => MarshalIn (ObjRef tt) where
  mIn = InMarshaller {
    mInFuncFld = \ptr -> MaybeT $ do
      objPtr <- peek (castPtr ptr)
      hndl <- hsqmlGetObjectHandle objPtr
      return $ if isNullObjectHandle hndl
        then Nothing else Just $ ObjRef hndl,
    mIOTypeFld = Tagged $ addPointer $ classType (classDefCAF :: ClassDef tt),
    mIOInitFld = Tagged $ do
      let cd = classDefCAF :: ClassDef tt
          initVar = classInit cd
      initFlag <- readIORef initVar
      if initFlag
        then return ()
        else do
          writeIORef initVar True
          void $ evaluate $ classHndl cd
  }

instance (Object tt) => MarshalThis (ObjRef tt) where
  type ThisObj (ObjRef tt) = tt
  mThis = ThisMarshaller {
      mThisFuncFld = \ptr -> do
      hndl <- hsqmlGetObjectHandle ptr
      return $ ObjRef hndl
  }

retagType :: Tagged (ObjRef tt) a -> Tagged tt a
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
    mIOTypeFld = retagType mIOType,
    mIOInitFld = retagType mIOInit
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
  hndl <- hsqmlCreateObject obj $ classHndl (classDefCAF :: ClassDef tt)
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
class (Typeable tt) => Object tt where
  classDef :: ClassDef tt

-- | Uninlinable version of classDef to try and ensure that class definitions
-- get stored as constant applicable forms.
{-# NOINLINE classDefCAF #-}
classDefCAF :: (Object tt) => ClassDef tt
classDefCAF = classDef

--
-- ClassDef
--

-- | Represents the API of the QML class which wraps the type @tt@.
data ClassDef tt = ClassDef {
  classType :: TypeName,
  classInit :: IORef Bool,
  classHndl :: HsQMLClassHandle
}

-- | Generates a 'ClassDef' from a list of 'Member's.
defClass :: forall tt. (Object tt) => [Member tt] -> ClassDef tt
defClass ms =
  let (name,init) = unsafePerformIO $
        untag (createClassName :: Tagged tt (IO (String, IORef Bool)))
      hndl = unsafePerformIO $ do
        writeIORef init True
        c <- createClass name ms
        return c
  in ClassDef (TypeName name) init hndl

createClassName :: forall tt. (Object tt) => Tagged tt (IO (String, IORef Bool))
createClassName = Tagged $ do
  id <- hsqmlGetNextClassId
  init <- newIORef False
  let typ  = typeOf (undefined :: tt)
      con  = typeRepTyCon typ
      name = showString (tyConModule con) $ showChar '.' $
          showString (tyConName con) $ showChar '_' $ showInt id ""
  return (name,init)

createClass :: forall tt. (Object tt) =>
  String -> [Member tt] -> IO HsQMLClassHandle
createClass name ms = do
  initMembers ms
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
  let err = error ("Failed to create QML class '"++name++"'.")
  return $ fromMaybe err hndl

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

-- | Call all the initialisation functions for a list of members.
initMembers :: [Member tt] -> IO ()
initMembers = mapM_ f
  where f (MethodMember m) = methodInit m
        f (PropertyMember m) = propertyInit m

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
  methodFunc  :: UniformFunc,
  methodInit  :: IO ()
}

data CrudeMethodTypes = CrudeMethodTypes {
    methodParamTypes :: [TypeName],
    methodReturnType :: TypeName,
    methodCrudeInit  :: IO ()
  }

crudeTypesToList :: CrudeMethodTypes -> [TypeName]
crudeTypesToList (CrudeMethodTypes p r _) = r:p

-- | Supports marshalling Haskell functions with an arbitrary number of
-- arguments.
class MethodSuffix a where
  mkMethodFunc  :: Int -> a -> Ptr (Ptr ()) -> ErrIO ()
  mkMethodTypes :: Tagged a CrudeMethodTypes

instance (MarshalIn a, MethodSuffix b) => MethodSuffix (a -> b) where
  mkMethodFunc n f pv = do
    ptr <- errIO $ peekElemOff pv n
    val <- mInFunc ptr
    mkMethodFunc (n+1) (f val) pv
    return ()
  mkMethodTypes =
    let (CrudeMethodTypes p r i) =
          untag (mkMethodTypes :: Tagged b CrudeMethodTypes)
        typ = untag (mIOType :: Tagged a TypeName)
        ini = untag (mIOInit :: Tagged a (IO ()))
    in Tagged $ CrudeMethodTypes (typ:p) r (ini >> i)

instance (MarshalOut a) => MethodSuffix (IO a) where
  mkMethodFunc _ f pv = errIO $ do
    ptr <- peekElemOff pv 0
    val <- f
    if nullPtr == ptr
    then return ()
    else mOutFunc ptr val
  mkMethodTypes =
    let typ = untag (mIOType :: Tagged a TypeName)
        ini = untag (mIOInit :: Tagged a (IO ()))
    in Tagged $ CrudeMethodTypes [] typ ini

mkUniformFunc :: forall tt ms. (MarshalThis tt, MethodSuffix ms) =>
  (tt -> ms) -> UniformFunc
mkUniformFunc f = \pt pv -> do
  this <- mThisFunc pt
  runErrIO $ mkMethodFunc 1 (f this) pv

-- | Defines a named method using a function @f@ in the IO monad.
--
-- The first argument to @f@ receives the \"this\" object and hence must match
-- the type of the class on which the method is being defined. Subsequently,
-- there may be zero or more parameter arguments followed by an optional return
-- argument in the IO monad. These argument types must be members of the
-- 'MarshalThis', 'MarshalIn', and 'MarshalOut' typeclasses respectively.

defMethod ::
  forall tt ms. (MarshalThis tt, MethodSuffix ms) =>
  String -> (tt -> ms) -> Member (ThisObj tt)
defMethod name f =
  let crude = untag (mkMethodTypes :: Tagged ms CrudeMethodTypes)
  in MethodMember $ Method name
       (crudeTypesToList crude) (mkUniformFunc f) (methodCrudeInit crude)

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
  propertyWriteFunc :: Maybe UniformFunc,
  propertyInit :: IO ()
}

-- | Defines a named read-only property using an accessor function in the IO
-- monad.
defPropertyRO ::
  forall tt tr. (MarshalThis tt, MarshalOut tr) =>
  String -> (tt -> IO tr) -> Member (ThisObj tt)
defPropertyRO name g = PropertyMember $ Property name
  (untag (mIOType :: Tagged tr TypeName))
  (mkUniformFunc g)
  Nothing
  (untag (mIOInit :: Tagged tr (IO ())))

-- | Defines a named read-write property using a pair of accessor and mutator
-- functions in the IO monad.
defPropertyRW ::
  forall tt tr. (MarshalThis tt, MarshalOut tr) =>
  String -> (tt -> IO tr) -> (tt -> tr -> IO ()) -> Member (ThisObj tt)
defPropertyRW name g s = PropertyMember $ Property name
  (untag (mIOType :: Tagged tr TypeName))
  (mkUniformFunc g)
  (Just $ mkUniformFunc s)
  (untag (mIOInit :: Tagged tr (IO ())))

--
-- Meta Object Compiler
--

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
