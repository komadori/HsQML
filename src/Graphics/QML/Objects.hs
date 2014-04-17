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

  -- * Dynamic Object References
  AnyObjRef,
  anyObjRef,
  fromAnyObjRef,

  -- * Customer Marshallers
  objSimpleMarshaller,
  objBidiMarshaller
) where

import System.IO

import Graphics.QML.Internal.BindCore
import Graphics.QML.Internal.BindObj
import Graphics.QML.Internal.JobQueue
import Graphics.QML.Internal.Marshal
import Graphics.QML.Internal.MetaObj
import Graphics.QML.Internal.Objects

import Control.Concurrent.MVar
import Control.Monad.Trans.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Tagged
import Data.Typeable
import Data.IORef
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import System.IO.Unsafe
import Numeric

--
-- ObjRef
--

-- | Represents an instance of the QML class which wraps the type @tt@.
data ObjRef tt = ObjRef {
  objHndl :: HsQMLObjectHandle
}

instance (Object tt) => Marshal (ObjRef tt) where
    type MarshalMode (ObjRef tt) = ModeObjBidi tt
    marshaller = Marshaller {
        mTypeCVal_ = Tagged tyObject,
        mFromCVal_ = \ptr -> do
            any <- mFromCVal ptr
            MaybeT $ return $ fromAnyObjRef any,
        mToCVal_ = \obj ptr ->
            mToCVal (AnyObjRef $ objHndl obj) ptr,
        mWithCVal_ = \obj f ->
            mWithCVal (AnyObjRef $ objHndl obj) f,
        mFromHndl_ = \hndl ->
            return $ ObjRef hndl,
        mToHndl_ = \obj ->
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

-- | Represents an instance of a QML class which wraps an arbitrary Haskell
-- type. Unlike 'ObjRef', an 'AnyObjRef' only carries the type of its Haskell
-- value dynamically and does not encode it into the static type.
data AnyObjRef = AnyObjRef {
  anyObjHndl :: HsQMLObjectHandle
}

instance Marshal AnyObjRef where
    type MarshalMode AnyObjRef = ModeObjBidi ()
    marshaller = Marshaller {
        mTypeCVal_ = Tagged tyObject,
        mFromCVal_ = \ptr -> MaybeT $ do
            objPtr <- peek (castPtr ptr)
            hndl <- hsqmlGetObjectHandle objPtr
            return $ if isNullObjectHandle hndl
                then Nothing else Just $ AnyObjRef hndl,
        mToCVal_ = \obj ptr -> do
            objPtr <- hsqmlObjectGetPointer $ anyObjHndl obj
            poke (castPtr ptr) objPtr,
        mWithCVal_ = \obj f ->
            alloca $ \(ptr :: Ptr (Ptr ())) ->
                flip mToCVal (castPtr ptr) obj >> f (castPtr ptr),
        mFromHndl_ = \hndl ->
            return $ AnyObjRef hndl,
        mToHndl_ = \obj ->
            return $ anyObjHndl obj}

-- | Upcasts an 'ObjRef' into an 'AnyObjRef'.
anyObjRef :: ObjRef tt -> AnyObjRef
anyObjRef (ObjRef hndl) = AnyObjRef hndl

-- | Attempts to downcast an 'AnyObjRef' into an 'ObjRef' with the specific
-- underlying Haskell type @tt@.
fromAnyObjRef :: (Object tt) => AnyObjRef -> Maybe (ObjRef tt)
fromAnyObjRef = unsafePerformIO . fromAnyObjRefIO

fromAnyObjRefIO :: forall tt. (Object tt) => AnyObjRef -> IO (Maybe (ObjRef tt))
fromAnyObjRefIO (AnyObjRef hndl) = do
    let srcRep = typeOf (undefined :: tt)
    dstRep <- hsqmlObjectGetHsTyperep hndl
    if srcRep == dstRep
        then return $ Just $ ObjRef hndl
        else return Nothing

-- | Provides a QML-to-Haskell 'Marshaller' which allows you to define
-- instances of 'Marshal' for custom 'Object' types. This allows a custom types
-- to be passed into Haskell code as method parameters without having to
-- manually deal with 'ObjRef's.
--
-- For example, an instance for 'MyObjectType' would be defined as follows:
--
-- @
-- instance Marshal MyObjectType where
--     type MarshalMode MyObjectType = ValObjFrom MyObjectType
--     marshaller = objSimpleMarshaller
-- @
objSimpleMarshaller ::
    forall obj. (Object obj) => Marshaller obj (ModeObjFrom obj)
objSimpleMarshaller = Marshaller {
    mTypeCVal_ = retag (mTypeCVal :: Tagged (ObjRef obj) TypeId),
    mFromCVal_ = \ptr -> (errIO . fromObjRefIO) =<< mFromCVal ptr,
    mToCVal_ = unimplToCVal,
    mWithCVal_ = unimplWithCVal,
    mFromHndl_ = hsqmlObjectGetHaskell,
    mToHndl_ = unimplToHndl}

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
--     type MarshalMode MyObjectType = ModeObjBidi MyObjectType
--     marshaller = objBidiMarshaller newObject
-- @
objBidiMarshaller ::
    forall obj. (Object obj) =>
    (obj -> IO (ObjRef obj)) -> Marshaller obj (ModeObjBidi obj) 
objBidiMarshaller newFn = Marshaller {
    mTypeCVal_ = retag (mTypeCVal :: Tagged (ObjRef obj) TypeId),
    mFromCVal_ = \ptr -> (errIO . fromObjRefIO) =<< mFromCVal ptr,
    mToCVal_ = \obj ptr -> flip mToCVal ptr =<< newFn obj,
    mWithCVal_ = \obj f -> flip mWithCVal f =<< newFn obj,
    mFromHndl_ = hsqmlObjectGetHaskell,
    mToHndl_ = fmap objHndl . newFn}

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
    (_, val) <- getFromMemoStore classRecDb typ (createClass typ cdef)
    return val

createClass :: forall tt. (Object tt) => TypeRep -> ClassDef tt -> IO ClassRec
createClass typRep cdef = do
  hsqmlInit
  classId <- hsqmlGetNextClassId
  let constrs t = typeRepTyCon t : (concatMap constrs $ typeRepArgs t)
      name = foldr (\c s -> showString (tyConName c) .
          showChar '_' . s) id (constrs typRep) $ showInt classId ""
      ms = classMembers cdef
      moc = compileClass name ms
      sigs = filterMembers SignalMember ms
      sigMap = Map.fromList $ flip zip [0..] $ map (fromJust . memberKey) sigs
      maybeMarshalFunc = maybe (return nullFunPtr) marshalFunc
  metaDataPtr <- crlToNewArray return (mData moc)
  metaStrInfoPtr <- crlToNewArray return (mStrInfo moc)
  metaStrCharPtr <- crlToNewArray return (mStrChar moc)
  methodsPtr <- crlToNewArray maybeMarshalFunc (mFuncMethods moc)
  propsPtr <- crlToNewArray maybeMarshalFunc (mFuncProperties moc)
  maybeHndl <- hsqmlCreateClass
      metaDataPtr metaStrInfoPtr metaStrCharPtr typRep methodsPtr propsPtr
  case maybeHndl of
      Just hndl -> return $ ClassRec hndl sigMap
      Nothing -> error ("Failed to create QML class '"++name++"'.")

--
-- Method
--

data MethodTypeInfo = MethodTypeInfo {
  methodParamTypes :: [TypeId],
  methodReturnType :: TypeId
}

newtype MSHelp a = MSHelp a

-- | Supports marshalling Haskell functions with an arbitrary number of
-- arguments.
class MethodSuffix a where
  mkMethodFunc  :: Int -> a -> Ptr (Ptr ()) -> ErrIO ()
  mkMethodTypes :: Tagged a MethodTypeInfo

instance (Marshal a, CanGetFrom a ~ Yes, MethodSuffix (MSHelp b)) =>
  MethodSuffix (MSHelp (a -> b)) where
  mkMethodFunc n (MSHelp f) pv = do
    ptr <- errIO $ peekElemOff pv n
    val <- mFromCVal ptr
    mkMethodFunc (n+1) (MSHelp $ f val) pv
    return ()
  mkMethodTypes =
    let (MethodTypeInfo p r) =
          untag (mkMethodTypes :: Tagged (MSHelp b) MethodTypeInfo)
        typ = untag (mTypeCVal :: Tagged a TypeId)
    in Tagged $ MethodTypeInfo (typ:p) r

instance (Marshal a, CanReturnTo a ~ Yes) =>
  MethodSuffix (MSHelp (IO a)) where
  mkMethodFunc _ (MSHelp f) pv = errIO $ do
    ptr <- peekElemOff pv 0
    val <- f
    if nullPtr == ptr
    then return ()
    else mToCVal val ptr
  mkMethodTypes =
    let typ = untag (mTypeCVal :: Tagged a TypeId)
    in Tagged $ MethodTypeInfo [] typ

mkUniformFunc :: forall tt ms.
  (Marshal tt, CanGetObjFrom tt ~ Yes, MethodSuffix (MSHelp ms)) =>
  (tt -> ms) -> UniformFunc
mkUniformFunc f = \pt pv -> do
  hndl <- hsqmlGetObjectHandle pt
  this <- mFromHndl hndl
  runErrIO $ mkMethodFunc 1 (MSHelp $ f this) pv

newtype VoidIO = VoidIO {runVoidIO :: (IO ())}

instance MethodSuffix (MSHelp VoidIO) where
    mkMethodFunc _ (MSHelp f) pv = errIO $ runVoidIO f
    mkMethodTypes = Tagged $ MethodTypeInfo [] tyVoid

class IsVoidIO a
instance (IsVoidIO b) => IsVoidIO (a -> b)
instance IsVoidIO VoidIO

mkSpecialFunc :: forall tt ms.
    (Marshal tt, CanGetObjFrom tt ~ Yes, MethodSuffix (MSHelp ms),
        IsVoidIO ms) => (tt -> ms) -> UniformFunc
mkSpecialFunc f = \pt pv -> do
    hndl <- hsqmlGetObjectHandle pt
    this <- mFromHndl hndl
    runErrIO $ mkMethodFunc 0 (MSHelp $ f this) pv

-- | Defines a named method using a function @f@ in the IO monad.
--
-- The first argument to @f@ receives the \"this\" object and hence must match
-- the type of the class on which the method is being defined. Subsequently,
-- there may be zero or more parameter arguments followed by an optional return
-- argument in the IO monad.
defMethod :: forall tt ms.
  (Marshal tt, CanGetObjFrom tt ~ Yes, MethodSuffix (MSHelp ms)) =>
  String -> (tt -> ms) -> Member (ThisObj tt)
defMethod name f =
  let crude = untag (mkMethodTypes :: Tagged (MSHelp ms) MethodTypeInfo)
  in Member MethodMember
       name
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
  forall tt tr. (Marshal tt, CanGetObjFrom tt ~ Yes,
    Marshal tr, CanReturnTo tr ~ Yes) =>
  String -> (tt -> IO tr) -> Member (ThisObj tt)
defPropertyRO name g = Member PropertyMember
  name
  (untag (mTypeCVal :: Tagged tr TypeId))
  []
  (mkUniformFunc g)
  Nothing
  Nothing

-- | Defines a named read-write property using a pair of accessor and mutator
-- functions in the IO monad.
defPropertyRW ::
  forall tt tr. (Marshal tt, CanGetObjFrom tt ~ Yes,
    Marshal tr, CanReturnTo tr ~ Yes, CanGetFrom tr ~ Yes) =>
  String -> (tt -> IO tr) -> (tt -> tr -> IO ()) -> Member (ThisObj tt)
defPropertyRW name g s = Member PropertyMember
  name
  (untag (mTypeCVal :: Tagged tr TypeId))
  []
  (mkUniformFunc g)
  (Just $ mkSpecialFunc (\a b -> VoidIO $ s a b))
  Nothing

--
-- Signal
--

data SignalTypeInfo = SignalTypeInfo {
  signalParamTypes :: [TypeId]
}

-- | Defines a named signal using a 'SignalKey'.
defSignal ::
  forall obj sk. (Object obj, SignalKey sk) => Tagged sk String -> Member obj
defSignal tn =
  let crude = untag (mkSignalTypes :: Tagged (SignalParams sk) SignalTypeInfo)
  in Member SignalMember
       (untag tn)
       tyVoid
       (signalParamTypes crude)
       (\_ _ -> return ())
       Nothing
       (Just $ typeOf (undefined :: sk))

-- | Fires a signal on an 'Object', specified using a 'SignalKey'.
fireSignal ::
    forall tt sk. (
        Marshal tt, CanPassObjTo tt ~ Yes,
        Object (ThisObj tt), SignalKey sk) =>
    Tagged sk tt -> SignalParams sk 
fireSignal this =
    let start cnt = do
           crec <- getClassRec (classDef :: ClassDef (ThisObj tt))
           let keyRep = typeOf (undefined :: sk)
               slotMay = Map.lookup keyRep $ crecSigs crec
           case slotMay of
                Just slotIdx -> postJob $ do
                    hndl <- mToHndl $ untag this
                    withActiveObject hndl $ cnt $ SignalData hndl slotIdx
                Nothing ->
                    error ("Attempt to fire undefined signal on class.")
        cont ps (SignalData hndl slotIdx) =
            withArray (nullPtr:ps) (\pptr ->
                hsqmlFireSignal hndl slotIdx pptr)
    in mkSignalArgs start cont

data SignalData = SignalData HsQMLObjectHandle Int

-- | Instances of the 'SignalKey' class identify distinct signals. The
-- associated 'SignalParams' type specifies the signal's signature.
class (SignalSuffix (SignalParams sk), Typeable sk) => SignalKey sk where
  type SignalParams sk

-- | Supports marshalling an arbitrary number of arguments into a QML signal.
class SignalSuffix ss where
    mkSignalArgs  :: forall usr.
        ((usr -> IO ()) -> IO ()) -> ([Ptr ()] -> usr -> IO ()) -> ss
    mkSignalTypes :: Tagged ss SignalTypeInfo

instance (Marshal a, CanPassTo a ~ Yes, SignalSuffix b) =>
    SignalSuffix (a -> b) where
    mkSignalArgs start cont param =
        mkSignalArgs start (\ps usr ->
            mWithCVal param (\ptr ->
                cont (ptr:ps) usr))
    mkSignalTypes =
        let (SignalTypeInfo p) =
                untag (mkSignalTypes :: Tagged b SignalTypeInfo)
            typ = untag (mTypeCVal :: Tagged a TypeId)
        in Tagged $ SignalTypeInfo (typ:p)

instance SignalSuffix (IO ()) where
    mkSignalArgs start cont =
        start $ cont []
    mkSignalTypes =
        Tagged $ SignalTypeInfo []
