{-# LANGUAGE
    ScopedTypeVariables,
    TypeFamilies,
    FlexibleContexts,
    FlexibleInstances
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

  -- * Signals
  defSignal,
  fireSignal,
  SignalKey (
    type SignalParams),
  SignalSuffix,

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
-- Counted Reverse List
--

data CRList a = CRList {
  crlLen  :: !Int,
  crlList :: [a]
}

crlEmpty :: CRList a
crlEmpty = CRList 0 []

crlAppend1 :: CRList a -> a -> CRList a
crlAppend1 (CRList n xs) x = CRList (n+1) (x:xs)

crlAppend :: CRList a -> [a] -> CRList a
crlAppend (CRList n xs) ys = CRList n' xs'
  where (xs', n')       = rev ys xs n
        rev []     vs n = (vs, n)
        rev (u:us) vs n = rev us (u:vs) (n+1)

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
  let moc = compileClass name ms
      maybeMarshalFunc = maybe (return nullFunPtr) marshalFunc
  metaDataPtr <- crlToNewArray return (mData moc)
  metaStrDataPtr <- crlToNewArray return (mStrData moc)
  methodsPtr <- crlToNewArray maybeMarshalFunc (mFuncMethods moc)
  propsPtr <- crlToNewArray maybeMarshalFunc (mFuncProperties moc)
  hsqmlInit
  hndl <- hsqmlCreateClass metaDataPtr metaStrDataPtr methodsPtr propsPtr
  let err = error ("Failed to create QML class '"++name++"'.")
  return $ fromMaybe err hndl

crlToNewArray :: (Storable b) => (a -> IO b) -> CRList a -> IO (Ptr b)
crlToNewArray f (CRList len lst) = do
  ptr <- mallocArray len
  pokeRev ptr lst len
  return ptr
  where pokeRev _ []     _ = return ()
        pokeRev p (x:xs) n = do
          let n' = n-1
          x' <- f x
          pokeElemOff p n' x'
          pokeRev p xs n'

--
-- Member
--

data MemberKind
  = MethodMember
  | PropertyMember
  | SignalMember
  deriving (Bounded, Enum, Eq)

-- | Represents a named member of the QML class which wraps type @tt@.
data Member tt = Member {
  memberKind   :: MemberKind,
  memberName   :: String,
  memberInit   :: Int -> IO (),
  memberType   :: TypeName,
  memberParams :: [TypeName],
  memberFun    :: UniformFunc,
  memberFunAux :: Maybe UniformFunc
}

filterMembers :: MemberKind -> [Member tt] -> [Member tt]
filterMembers k ms =
  filter (\m -> k == memberKind m) ms

-- | Call all the initialisation functions for a list of members.
initMembers :: [Member tt] -> IO ()
initMembers ms =
  mapM_ (\k ->
    mapM_ (\(i,m) -> memberInit m i) $ zip [0..] $ filterMembers k ms) $
  enumFromTo minBound maxBound

--
-- Method
--

data MethodTypeInfo = MethodTypeInfo {
  methodParamTypes :: [TypeName],
  methodReturnType :: TypeName,
  methodTypesInit  :: IO ()
}

-- | Supports marshalling Haskell functions with an arbitrary number of
-- arguments.
class MethodSuffix a where
  mkMethodFunc  :: Int -> a -> Ptr (Ptr ()) -> ErrIO ()
  mkMethodTypes :: Tagged a MethodTypeInfo

instance (MarshalIn a, MethodSuffix b) => MethodSuffix (a -> b) where
  mkMethodFunc n f pv = do
    ptr <- errIO $ peekElemOff pv n
    val <- mInFunc ptr
    mkMethodFunc (n+1) (f val) pv
    return ()
  mkMethodTypes =
    let (MethodTypeInfo p r i) =
          untag (mkMethodTypes :: Tagged b MethodTypeInfo)
        typ = untag (mIOType :: Tagged a TypeName)
        ini = untag (mIOInit :: Tagged a (IO ()))
    in Tagged $ MethodTypeInfo (typ:p) r (ini >> i)

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
    in Tagged $ MethodTypeInfo [] typ ini

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
  let crude = untag (mkMethodTypes :: Tagged ms MethodTypeInfo)
  in Member MethodMember
       name
       (const $ methodTypesInit crude)
       (methodReturnType crude)
       (methodParamTypes crude)
       (mkUniformFunc f)
       Nothing

--
-- Property
--

-- | Defines a named read-only property using an accessor function in the IO
-- monad.
defPropertyRO ::
  forall tt tr. (MarshalThis tt, MarshalOut tr) =>
  String -> (tt -> IO tr) -> Member (ThisObj tt)
defPropertyRO name g = Member PropertyMember
  name
  (const $ untag (mIOInit :: Tagged tr (IO ())))
  (untag (mIOType :: Tagged tr TypeName))
  []
  (mkUniformFunc g)
  Nothing

-- | Defines a named read-write property using a pair of accessor and mutator
-- functions in the IO monad.
defPropertyRW ::
  forall tt tr. (MarshalThis tt, MarshalOut tr) =>
  String -> (tt -> IO tr) -> (tt -> tr -> IO ()) -> Member (ThisObj tt)
defPropertyRW name g s = Member PropertyMember
  name
  (const $ untag (mIOInit :: Tagged tr (IO ())))
  (untag (mIOType :: Tagged tr TypeName))
  []
  (mkUniformFunc g)
  (Just $ mkUniformFunc s)

--
-- Signal
--

data SignalTypeInfo = SignalTypeInfo {
  signalParamTypes :: [TypeName],
  signalTypesInit  :: IO ()
}

-- | Defines a named signal using a 'SignalKey'.
defSignal ::
  forall obj sk. (Object obj, SignalKey sk) => Tagged sk String -> Member obj
defSignal tn =
  let crude = untag (mkSignalTypes :: Tagged (SignalParams sk) SignalTypeInfo)
      slot = untag (signalSlotCAF :: Tagged (obj, sk) (IORef (Maybe Int)))
  in Member SignalMember
       (untag tn)
       (\idx -> signalSlotInit slot idx >> signalTypesInit crude)
       (TypeName "")
       (signalParamTypes crude)
       (\_ _ -> return ())
       Nothing

{-# NOINLINE signalSlotCAF #-}
signalSlotCAF ::
  (Object obj, SignalKey sk) =>
  Tagged (obj, sk) (IORef (Maybe Int))
signalSlotCAF = Tagged $ unsafePerformIO $ newIORef Nothing

signalSlotInit :: IORef (Maybe Int) -> Int -> IO ()
signalSlotInit ref idx =
  writeIORef ref (Just idx)

-- | Fires a signal on an 'Object', specified using a 'SignalKey'.
fireSignal ::
  forall obj sk. (Object obj, SignalKey sk) =>
  Tagged sk (ObjRef obj) -> SignalParams sk 
fireSignal this =
  let slotRef = untag $ (signalSlotCAF ::
        Tagged (obj, sk) (IORef (Maybe Int)))
      cont ps = do
        slotMay <- readIORef slotRef
        case slotMay of
          Just slotIdx -> withArray (nullPtr:ps) (\pptr ->
            hsqmlFireSignal (objHndl $ untag this) slotIdx pptr)
          Nothing -> error ("Attempt to fire undefined signal on class '"++
            (typeName $ classType $ (classDefCAF :: ClassDef obj))++"'.")
  in mkSignalArgs cont

-- | Instances of the 'SignalKey' class identify distinct signals. The
-- associated 'SignalParams' type specifies the signal's signature.
class (SignalSuffix (SignalParams sk)) => SignalKey sk where
  type SignalParams sk

-- | Supports marshalling an arbitrary number of arguments into a QML signal.
class SignalSuffix ss where
  mkSignalArgs  :: ([Ptr ()] -> IO ()) -> ss
  mkSignalTypes :: Tagged ss SignalTypeInfo

instance (MarshalOut a, SignalSuffix b) => SignalSuffix (a -> b) where
  mkSignalArgs cont param =
    mkSignalArgs (\ps ->
      mOutAlloc param (\ptr ->
        cont (ptr:ps)))
  mkSignalTypes =
    let (SignalTypeInfo p i) =
          untag (mkSignalTypes :: Tagged b SignalTypeInfo)
        typ = untag (mIOType :: Tagged a TypeName)
        ini = untag (mIOInit :: Tagged a (IO ()))
    in Tagged $ SignalTypeInfo (typ:p) (ini >> i)

instance SignalSuffix (IO ()) where
  mkSignalArgs cont =
    cont []
  mkSignalTypes =
    Tagged $ SignalTypeInfo [] (return ())

--
-- Meta Object Compiler
--

data MOCState = MOCState {
  mData            :: CRList CUInt,
  mDataMethodsIdx  :: Maybe Int,
  mDataPropsIdx    :: Maybe Int,
  mStrData         :: CRList CChar,
  mStrDataMap      :: Map String CUInt,
  mFuncMethods     :: CRList (Maybe UniformFunc),
  mFuncProperties  :: CRList (Maybe UniformFunc),
  mMethodCount     :: Int,
  mSignalCount     :: Int,
  mPropertyCount   :: Int
}

-- | Generate MOC meta-data from a class name and member list.
compileClass :: String -> [Member tt] -> MOCState
compileClass name ms = 
  let enc = flip execState newMOCState $ do
        writeInt 5                           -- Revision
        writeString name                     -- Class name
        writeInt 0 >> writeInt 0             -- Class info
        writeIntegral $
          mMethodCount enc +
          mSignalCount enc                   -- Methods
        writeIntegral $
          fromMaybe 0 $ mDataMethodsIdx enc  -- Methods (data index)
        writeIntegral $ mPropertyCount enc   -- Properties
        writeIntegral $
          fromMaybe 0 $ mDataPropsIdx enc    -- Properties (data index)
        writeInt 0 >> writeInt 0             -- Enums
        writeInt 0 >> writeInt 0             -- Constructors
        writeInt 0                           -- Flags
        writeIntegral $ mSignalCount enc     -- Signals
        mapM_ writeMethod $ filterMembers SignalMember ms
        mapM_ writeMethod $ filterMembers MethodMember ms
        mapM_ writeProperty $ filterMembers PropertyMember ms
        writeInt 0
  in enc

newMOCState :: MOCState
newMOCState =
  MOCState crlEmpty Nothing Nothing crlEmpty Map.empty crlEmpty crlEmpty 0 0 0

writeInt :: CUInt -> State MOCState ()
writeInt int = do
  state <- get
  put $ state {mData = mData state `crlAppend1` int}
  return ()

writeIntegral :: (Integral a) => a -> State MOCState ()
writeIntegral int =
  writeInt (fromIntegral int)

writeString :: String -> State MOCState ()
writeString str = do
  state <- get
  let msd    = mStrData state
      msdMap = mStrDataMap state
  case (Map.lookup str msdMap) of
    Just idx -> writeInt idx
    Nothing  -> do
      let idx = crlLen msd
          msd' = msd `crlAppend` (map castCharToCChar str) `crlAppend1` 0
          msdMap' = Map.insert str (fromIntegral idx) msdMap
      put $ state {
        mStrData = msd',
        mStrDataMap = msdMap'}
      writeIntegral idx

writeMethod :: Member tt -> State MOCState ()
writeMethod m = do
  idx <- get >>= return . crlLen . mData
  writeString $ methodSignature m
  writeString $ methodParameters m
  writeString $ typeName $ memberType m
  writeString ""
  let (mc,sc,flags) = case memberKind m of
        SignalMember -> (0,1,mfMethodSignal)
        _            -> (1,0,0)
  writeInt (mfAccessPublic .|. mfMethodScriptable .|. flags)
  state <- get
  put $ state {
    mDataMethodsIdx = mplus (mDataMethodsIdx state) (Just idx),
    mMethodCount = mc + (mMethodCount state),
    mSignalCount = sc + (mSignalCount state),
    mFuncMethods = mFuncMethods state `crlAppend1` (Just $ memberFun m)}
  return ()

writeProperty :: Member tt -> State MOCState ()
writeProperty p = do
  idx <- get >>= return . crlLen . mData
  writeString $ memberName p
  writeString $ typeName $ memberType p
  writeInt (pfReadable .|. pfScriptable .|.
    if (isJust $ memberFunAux p) then pfWritable else 0)
  state <- get
  put $ state {
    mDataPropsIdx = mplus (mDataPropsIdx state) (Just idx),
    mPropertyCount = 1 + (mPropertyCount state),
    mFuncProperties = mFuncProperties state
      `crlAppend1` (Just $ memberFun p) `crlAppend1` memberFunAux p
  }
  return ()

foldr0 :: (a -> a -> a) -> a -> [a] -> a
foldr0 _ x [] = x
foldr0 f _ xs = foldr1 f xs

methodSignature :: Member tt -> String
methodSignature method =
  let paramTypes = memberParams method
  in (showString (memberName method) . showChar '(' .
       foldr0 (\l r -> l . showChar ',' . r) id
         (map (showString . typeName) paramTypes) . showChar ')') ""

methodParameters :: Member tt -> String
methodParameters method =
  replicate (flip (-) 1 $ length $ memberParams method) ','
