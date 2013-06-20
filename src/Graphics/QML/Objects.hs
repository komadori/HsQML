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

  -- * Customer Marshallers
  objSimpleMarshaller,
  objBidiMarshaller
) where

import Graphics.QML.Internal.BindObj
import Graphics.QML.Internal.Marshal
import Graphics.QML.Internal.Objects
import Graphics.QML.Internal.Engine

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Exception
import Data.Bits
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

-- | Represents an instance of the QML class which wraps the type @tt@.
data ObjRef tt = ObjRef {
  objHndl :: HsQMLObjectHandle
}

addPointer :: TypeName -> TypeName
addPointer (TypeName name) = TypeName $ name ++ "*"

instance (Object tt) => Marshal (ObjRef tt) where
  type MarshalMode (ObjRef tt) = ValObjBidi tt
  marshaller = MValObjBidi {
    mValObjBidi_typeName = Tagged $
      addPointer $ TypeName $
      unsafePerformIO $ getClassName (classDef :: ClassDef tt),
    mValObjBidi_typeInit = Tagged $
      void $ getClassRec (classDef :: ClassDef tt),
    mValObjBidi_valToHs = \ptr -> MaybeT $ do
      objPtr <- peek (castPtr ptr)
      hndl <- hsqmlGetObjectHandle objPtr
      return $ if isNullObjectHandle hndl
        then Nothing else Just $ ObjRef hndl,
    mValObjBidi_hsToVal = \obj ptr -> do
      objPtr <- hsqmlObjectGetPointer $ objHndl obj
      poke (castPtr ptr) objPtr,
    mValObjBidi_hsToAlloc = \obj f ->
      alloca $ \(ptr :: Ptr (Ptr ())) ->
        flip mHsToVal (castPtr ptr) obj >> f (castPtr ptr),
    mValObjBidi_objToHs = \hndl ->
      return $ ObjRef hndl,
    mValObjBidi_hsToObj = \obj ->
      return $ objHndl obj}

-- | Creates an instance of a QML class given a value of the underlying Haskell 
-- type @tt@.
newObject :: forall tt. (Object tt) => tt -> IO (ObjRef tt)
newObject obj = do
  cRec <- getClassRec (classDef :: ClassDef tt)
  oHndl <- hsqmlCreateObject obj $ crecHndl cRec
  return $ ObjRef oHndl

-- | Returns the associated value of the underlying Haskell type @tt@ from an
-- instance of the QML class which wraps it.
fromObjRef :: ObjRef tt -> tt
fromObjRef =
    unsafePerformIO . fromObjRefIO

fromObjRefIO :: ObjRef tt -> IO tt
fromObjRefIO =
    hsqmlObjectGetHaskell . objHndl 

-- | Provides a QML-to-Haskell 'Marshaller' which allows you to define
-- instances of 'Marshal' for custom 'Object' types. This allows a custom types
-- to be passed into Haskell code as method parameters without having to
-- manually deal with 'ObjRef's.
--
-- For example, an instance for 'MyObjectType' would be defined as follows:
--
-- @
-- instance Marshal MyObjectType where
--     type MarshalMode MyObjectType = ValObjToOnly MyObjectType
--     marshaller = objSimpleMarshaller
-- @
objSimpleMarshaller ::
  forall obj. (Object obj) => Marshaller obj (ValObjToOnly obj)
objSimpleMarshaller = MValObjToOnly {
  mValObjToOnly_typeName = retag (mTypeName :: Tagged (ObjRef obj) TypeName),
  mValObjToOnly_typeInit = retag (mTypeInit :: Tagged (ObjRef obj) (IO ())),
  mValObjToOnly_valToHs = \ptr -> (errIO . fromObjRefIO) =<< mValToHs ptr,
  mValObjToOnly_objToHs = hsqmlObjectGetHaskell}

-- | Provides a bidirectional QML-to-Haskell and Haskell-to-QML 'Marshaller'
-- which allows you to define instances of 'Marshal' for custom object types.
-- This allows a custom type to be passed in and out of Haskell code via
-- methods, properties, and signals, without having to manually deal with
-- 'ObjRef's. Unlike the simple marshaller, this one must be given a function
-- which specifies how to obtain an 'ObjRef' given a Haskell value.
--
-- For example, an instance for 'MyObjectType' which simply creates a new
-- object whenever one is required would be defined as follows:
--
-- @
-- instance Marshal MyObjectType where
--     type MarshalMode MyObjectType = ValObjBidi MyObjectType
--     marshaller = objBidiMarshaller newObject
-- @
objBidiMarshaller ::
  forall obj. (Object obj) =>
  (obj -> IO (ObjRef obj)) -> Marshaller obj (ValObjBidi obj) 
objBidiMarshaller newFn = MValObjBidi {
  mValObjBidi_typeName = retag (mTypeName :: Tagged (ObjRef obj) TypeName),
  mValObjBidi_typeInit = retag (mTypeInit :: Tagged (ObjRef obj) (IO ())),
  mValObjBidi_valToHs = \ptr -> (errIO . fromObjRefIO) =<< mValToHs ptr,
  mValObjBidi_hsToVal = \obj ptr -> flip mHsToVal ptr =<< newFn obj,
  mValObjBidi_hsToAlloc = \obj f -> flip mHsToAlloc f =<< newFn obj,
  mValObjBidi_objToHs = hsqmlObjectGetHaskell,
  mValObjBidi_hsToObj = fmap objHndl . newFn}

--
-- ClassDef
--

-- | Generates a 'ClassDef' from a list of 'Member's.
defClass :: forall tt. (Object tt) => [Member tt] -> ClassDef tt
defClass = ClassDef

data MemoStore k v = MemoStore (MVar (Map k v)) (IORef (Map k v))

newMemoStore :: IO (MemoStore k v)
newMemoStore = do
    let m = Map.empty
    mr <- newMVar m
    ir <- newIORef m
    return $ MemoStore mr ir

getFromMemoStore :: (Ord k) => MemoStore k v -> k -> IO v -> IO (Bool, v)
getFromMemoStore (MemoStore mr ir) key fn = do
    fstMap <- readIORef ir
    case Map.lookup key fstMap of
        Just val -> return (False, val)
        Nothing  -> modifyMVar mr $ \sndMap -> do
            case Map.lookup key sndMap of
                Just val -> return (sndMap, (False, val))
                Nothing  -> do
                    val <- fn
                    let newMap = Map.insert key val sndMap
                    writeIORef ir newMap
                    return (newMap, (True, val))

{-# NOINLINE classNameDb #-}
classNameDb :: MemoStore TypeRep String
classNameDb = unsafePerformIO $ newMemoStore

getClassName :: forall tt. (Object tt) => ClassDef tt -> IO String
getClassName _ =
    fmap snd $ getFromMemoStore classNameDb typ (createClassName typ)
    where typ = typeOf (undefined :: tt)

createClassName :: TypeRep -> IO String
createClassName typRep = do
    classId <- hsqmlGetNextClassId
    let constrs t = typeRepTyCon t : (concatMap constrs $ typeRepArgs t)
    return $ foldr (\c s -> showString (tyConName c) .
        showChar '_' . s) id (constrs typRep) $ showInt classId ""

data ClassRec = ClassRec {
    crecHndl :: HsQMLClassHandle,
    crecSigs :: Map TypeRep Int
}

{-# NOINLINE classRecDb #-}
classRecDb :: MemoStore TypeRep ClassRec
classRecDb = unsafePerformIO $ newMemoStore

getClassRec :: forall tt. (Object tt) => ClassDef tt -> IO ClassRec
getClassRec cdef = do
    let typ = typeOf (undefined :: tt)
    (new, val) <- getFromMemoStore classRecDb typ (createClass typ cdef)
    if new then initMembers $ classMembers cdef else return ()
    return val

createClass :: forall tt. (Object tt) => TypeRep -> ClassDef tt -> IO ClassRec
createClass typRep cdef = do
  name <- getClassName cdef
  let ms = classMembers cdef
      moc = compileClass name ms
      sigs = filterMembers SignalMember ms
      sigMap = Map.fromList $ flip zip [0..] $ map (fromJust . memberKey) sigs
      maybeMarshalFunc = maybe (return nullFunPtr) marshalFunc
  metaDataPtr <- crlToNewArray return (mData moc)
  metaStrDataPtr <- crlToNewArray return (mStrData moc)
  methodsPtr <- crlToNewArray maybeMarshalFunc (mFuncMethods moc)
  propsPtr <- crlToNewArray maybeMarshalFunc (mFuncProperties moc)
  hsqmlInit
  maybeHndl <- hsqmlCreateClass metaDataPtr metaStrDataPtr methodsPtr propsPtr
  case maybeHndl of
      Just hndl -> return $ ClassRec hndl sigMap
      Nothing -> error ("Failed to create QML class '"++name++"'.")

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

filterMembers :: MemberKind -> [Member tt] -> [Member tt]
filterMembers k ms =
  filter (\m -> k == memberKind m) ms

-- | Call all the initialisation functions for a list of members.
initMembers :: [Member tt] -> IO ()
initMembers = mapM_ memberInit

--
-- Method
--

data MethodTypeInfo = MethodTypeInfo {
  methodParamTypes :: [TypeName],
  methodReturnType :: TypeName,
  methodTypesInit  :: IO ()
}

newtype MSHelp a = MSHelp a

-- | Supports marshalling Haskell functions with an arbitrary number of
-- arguments.
class MethodSuffix a where
  mkMethodFunc  :: Int -> a -> Ptr (Ptr ()) -> ErrIO ()
  mkMethodTypes :: Tagged a MethodTypeInfo

instance (Marshal a, MarshalToHs (MarshalMode a), MethodSuffix (MSHelp b)) =>
  MethodSuffix (MSHelp (a -> b)) where
  mkMethodFunc n (MSHelp f) pv = do
    ptr <- errIO $ peekElemOff pv n
    val <- mValToHs ptr
    mkMethodFunc (n+1) (MSHelp $ f val) pv
    return ()
  mkMethodTypes =
    let (MethodTypeInfo p r i) =
          untag (mkMethodTypes :: Tagged (MSHelp b) MethodTypeInfo)
        typ = untag (mTypeName :: Tagged a TypeName)
        ini = untag (mTypeInit :: Tagged a (IO ()))
    in Tagged $ MethodTypeInfo (typ:p) r (ini >> i)

instance (Marshal a, MarshalToValRaw (MarshalMode a)) =>
  MethodSuffix (MSHelp (IO a)) where
  mkMethodFunc _ (MSHelp f) pv = errIO $ do
    ptr <- peekElemOff pv 0
    val <- f
    if nullPtr == ptr
    then return ()
    else mHsToVal val ptr
  mkMethodTypes =
    let typ = untag (mTypeName :: Tagged a TypeName)
        ini = untag (mTypeInit :: Tagged a (IO ()))
    in Tagged $ MethodTypeInfo [] typ ini

mkUniformFunc :: forall tt ms.
  (Marshal tt, MarshalFromObj (MarshalMode tt), MethodSuffix (MSHelp ms)) =>
  (tt -> ms) -> UniformFunc
mkUniformFunc f = \pt pv -> do
  hndl <- hsqmlGetObjectHandle pt
  this <- mObjToHs hndl
  runErrIO $ mkMethodFunc 1 (MSHelp $ f this) pv

-- | Defines a named method using a function @f@ in the IO monad.
--
-- The first argument to @f@ receives the \"this\" object and hence must match
-- the type of the class on which the method is being defined. Subsequently,
-- there may be zero or more parameter arguments followed by an optional return
-- argument in the IO monad.
defMethod :: forall tt ms.
  (Marshal tt, MarshalFromObj (MarshalMode tt), MethodSuffix (MSHelp ms)) =>
  String -> (tt -> ms) -> Member (ThisObj tt)
defMethod name f =
  let crude = untag (mkMethodTypes :: Tagged (MSHelp ms) MethodTypeInfo)
  in Member MethodMember
       name
       (methodTypesInit crude)
       (methodReturnType crude)
       (methodParamTypes crude)
       (mkUniformFunc f)
       Nothing
       Nothing

--
-- Property
--

-- | Defines a named read-only property using an accessor function in the IO
-- monad.
defPropertyRO ::
  forall tt tr. (Marshal tt, MarshalFromObj (MarshalMode tt),
    Marshal tr, MarshalToVal (MarshalMode tr)) =>
  String -> (tt -> IO tr) -> Member (ThisObj tt)
defPropertyRO name g = Member PropertyMember
  name
  (untag (mTypeInit :: Tagged tr (IO ())))
  (untag (mTypeName :: Tagged tr TypeName))
  []
  (mkUniformFunc g)
  Nothing
  Nothing

-- | Defines a named read-write property using a pair of accessor and mutator
-- functions in the IO monad.
defPropertyRW ::
  forall tt tr. (Marshal tt, MarshalFromObj (MarshalMode tt),
    Marshal tr, MarshalToHs (MarshalMode tr), MarshalToVal (MarshalMode tr)) =>
  String -> (tt -> IO tr) -> (tt -> tr -> IO ()) -> Member (ThisObj tt)
defPropertyRW name g s = Member PropertyMember
  name
  (untag (mTypeInit :: Tagged tr (IO ())))
  (untag (mTypeName :: Tagged tr TypeName))
  []
  (mkUniformFunc g)
  (Just $ mkUniformFunc s)
  Nothing

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
  in Member SignalMember
       (untag tn)
       (signalTypesInit crude)
       (TypeName "")
       (signalParamTypes crude)
       (\_ _ -> return ())
       Nothing
       (Just $ typeOf (undefined :: sk))

-- | Fires a signal on an 'Object', specified using a 'SignalKey'.
fireSignal ::
    forall tt sk. (Marshal tt, MarshalToObj (MarshalMode tt), SignalKey sk) =>
    Tagged sk tt -> SignalParams sk 
fireSignal this =
   let cont ps = do
           crec <- getClassRec (classDef :: ClassDef (ThisObj tt))
           let keyRep = typeOf (undefined :: sk)
               slotMay = Map.lookup keyRep $ crecSigs crec
           case slotMay of
                Just slotIdx -> withArray (nullPtr:ps) (\pptr -> do
                    hndl <- mHsToObj $ untag this
                    hsqmlFireSignal hndl slotIdx pptr)
                Nothing ->
                    error ("Attempt to fire undefined signal on class '"++
                    (typeName $ untag (mTypeName :: Tagged tt TypeName))++"'.")
    in mkSignalArgs cont

-- | Instances of the 'SignalKey' class identify distinct signals. The
-- associated 'SignalParams' type specifies the signal's signature.
class (SignalSuffix (SignalParams sk), Typeable sk) => SignalKey sk where
  type SignalParams sk

-- | Supports marshalling an arbitrary number of arguments into a QML signal.
class SignalSuffix ss where
  mkSignalArgs  :: ([Ptr ()] -> IO ()) -> ss
  mkSignalTypes :: Tagged ss SignalTypeInfo

instance (Marshal a, MarshalToVal (MarshalMode a), SignalSuffix b) =>
  SignalSuffix (a -> b) where
  mkSignalArgs cont param =
    mkSignalArgs (\ps ->
      mHsToAlloc param (\ptr ->
        cont (ptr:ps)))
  mkSignalTypes =
    let (SignalTypeInfo p i) =
          untag (mkSignalTypes :: Tagged b SignalTypeInfo)
        typ = untag (mTypeName :: Tagged a TypeName)
        ini = untag (mTypeInit :: Tagged a (IO ()))
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
