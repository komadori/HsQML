{-# LANGUAGE
    ScopedTypeVariables,
    TypeFamilies,
    FlexibleContexts,
    FlexibleInstances,
    LiberalTypeSynonyms
  #-}

-- | Facilities for defining new object types which can be marshalled between
-- Haskell and QML.
module Graphics.QML.Objects (
  -- * Object References
  ObjRef,
  newObject,
  newObjectDC,
  fromObjRef,

  -- * Dynamic Object References
  AnyObjRef,
  anyObjRef,
  fromAnyObjRef,

  -- * Class Definition
  Class,
  newClass,
  DefaultClass (
    classMembers),
  Member,

  -- * Methods
  defMethod,
  defMethod',
  MethodSuffix,

  -- * Signals
  defSignal,
  fireSignal,
  SignalKey,
  newSignalKey,
  SignalKeyClass (
    type SignalParams),
  SignalSuffix,

  -- * Properties
  defPropertyConst,
  defPropertyRO,
  defPropertySigRO,
  defPropertyRW,
  defPropertySigRW,
  defPropertyConst',
  defPropertyRO',
  defPropertySigRO',
  defPropertyRW',
  defPropertySigRW'
) where

import Graphics.QML.Internal.BindCore
import Graphics.QML.Internal.BindObj
import Graphics.QML.Internal.JobQueue
import Graphics.QML.Internal.Marshal
import Graphics.QML.Internal.MetaObj
import Graphics.QML.Internal.Objects
import Graphics.QML.Internal.Types

import Control.Concurrent.MVar
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.Proxy
import Data.Tagged
import Data.Typeable
import Data.IORef
import Data.Unique
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import System.IO.Unsafe
import Numeric

--
-- ObjRef
--

-- | Creates a QML object given a 'Class' and a Haskell value of type @tt@.
newObject :: forall tt. Class tt -> tt -> IO (ObjRef tt)
newObject (Class cHndl) obj =
  fmap ObjRef $ hsqmlCreateObject obj cHndl

-- | Creates a QML object given a Haskell value of type @tt@ which has a
-- 'DefaultClass' instance.
newObjectDC :: forall tt. (DefaultClass tt) => tt -> IO (ObjRef tt)
newObjectDC obj = do
  clazz <- getDefaultClass :: IO (Class tt)
  newObject clazz obj

-- | Returns the associated value of the underlying Haskell type @tt@ from an
-- instance of the QML class which wraps it.
fromObjRef :: ObjRef tt -> tt
fromObjRef = unsafeDupablePerformIO . fromObjRefIO

-- | Upcasts an 'ObjRef' into an 'AnyObjRef'.
anyObjRef :: ObjRef tt -> AnyObjRef
anyObjRef (ObjRef hndl) = AnyObjRef hndl

-- | Attempts to downcast an 'AnyObjRef' into an 'ObjRef' with the specific
-- underlying Haskell type @tt@.
fromAnyObjRef :: (Typeable tt) => AnyObjRef -> Maybe (ObjRef tt)
fromAnyObjRef = unsafeDupablePerformIO . fromAnyObjRefIO

--
-- Class
--

-- | Represents a QML class which wraps the type @tt@.
newtype Class tt = Class HsQMLClassHandle

-- | Creates a new QML class for the type @tt@.
newClass :: forall tt. (Typeable tt) => [Member tt] -> IO (Class tt)
newClass = fmap Class . createClass (typeOf (undefined :: tt))

createClass :: forall tt. TypeRep -> [Member tt] -> IO HsQMLClassHandle
createClass typRep ms = do
  hsqmlInit
  classId <- hsqmlGetNextClassId
  let constrs t = typeRepTyCon t : (concatMap constrs $ typeRepArgs t)
      name = foldr (\c s -> showString (tyConName c) .
          showChar '_' . s) id (constrs typRep) $ showInt classId ""
      ms' = ms ++ implicitSignals ms
      moc = compileClass name ms'
      sigs = filterMembers SignalMember ms'
      sigMap = Map.fromList $ flip zip [0..] $ map (fromJust . memberKey) sigs
      info = ClassInfo typRep sigMap
      maybeMarshalFunc = maybe (return nullFunPtr) marshalFunc
  metaDataPtr <- crlToNewArray return (mData moc)
  metaStrInfoPtr <- crlToNewArray return (mStrInfo moc)
  metaStrCharPtr <- crlToNewArray return (mStrChar moc)
  methodsPtr <- crlToNewArray maybeMarshalFunc (mFuncMethods moc)
  propsPtr <- crlToNewArray maybeMarshalFunc (mFuncProperties moc)
  maybeHndl <- hsqmlCreateClass
      metaDataPtr metaStrInfoPtr metaStrCharPtr info methodsPtr propsPtr
  case maybeHndl of
      Just hndl -> return hndl
      Nothing -> error ("Failed to create QML class '"++name++"'.")

implicitSignals :: [Member tt] -> [Member tt]
implicitSignals ms =
    let sigKeys = Set.fromList $ mapMaybe memberKey $
            filterMembers SignalMember ms
        impKeys = filter (flip Set.notMember sigKeys) $ mapMaybe memberKey $
            filterMembers PropertyMember ms
        impMember i k = Member SignalMember
            ("__implictSignal" ++ show i)
            tyVoid
            []
            (\_ _ -> return ())
            Nothing
            (Just k)
    in map (uncurry impMember) $ zip [(0::Int)..] impKeys

--
-- Default Class
--

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

-- | The class 'DefaultClass' specifies a standard class definition for the
-- type @tt@.
class (Typeable tt) => DefaultClass tt where
    -- | List of default class members.
    classMembers :: [Member tt]

{-# NOINLINE defaultClassDb #-}
defaultClassDb :: MemoStore TypeRep HsQMLClassHandle
defaultClassDb = unsafePerformIO $ newMemoStore

getDefaultClass :: forall tt. (DefaultClass tt) => IO (Class tt)
getDefaultClass = do
    let typ = typeOf (undefined :: tt)
    (_, val) <- getFromMemoStore defaultClassDb typ $
        createClass typ (classMembers :: [Member tt])
    return (Class val)

--
-- Method
--

data MethodTypeInfo = MethodTypeInfo {
  methodParamTypes :: [TypeId],
  methodReturnType :: TypeId
}

-- | Supports marshalling Haskell functions with an arbitrary number of
-- arguments.
class MethodSuffix a where
  mkMethodFunc  :: Int -> a -> Ptr (Ptr ()) -> ErrIO ()
  mkMethodTypes :: Tagged a MethodTypeInfo

instance (Marshal a, CanGetFrom a ~ Yes, MethodSuffix b) =>
  MethodSuffix (a -> b) where
  mkMethodFunc n f pv = do
    ptr <- errIO $ peekElemOff pv n
    val <- mFromCVal ptr
    mkMethodFunc (n+1) (f val) pv
    return ()
  mkMethodTypes =
    let (MethodTypeInfo p r) =
          untag (mkMethodTypes :: Tagged b MethodTypeInfo)
        typ = untag (mTypeCVal :: Tagged a TypeId)
    in Tagged $ MethodTypeInfo (typ:p) r

instance (Marshal a, CanReturnTo a ~ Yes) =>
  MethodSuffix (IO a) where
  mkMethodFunc _ f pv = errIO $ do
    ptr <- peekElemOff pv 0
    val <- f
    if nullPtr == ptr
    then return ()
    else mToCVal val ptr
  mkMethodTypes =
    let typ = untag (mTypeCVal :: Tagged a TypeId)
    in Tagged $ MethodTypeInfo [] typ

mkUniformFunc :: forall tt ms.
  (Marshal tt, CanGetFrom tt ~ Yes, IsObjType tt ~ Yes,
    MethodSuffix ms) =>
  (tt -> ms) -> UniformFunc
mkUniformFunc f = \pt pv -> do
  hndl <- hsqmlGetObjectFromPointer pt
  this <- mFromHndl hndl
  runErrIO $ mkMethodFunc 1 (f this) pv

newtype VoidIO = VoidIO {runVoidIO :: (IO ())}

instance MethodSuffix VoidIO where
    mkMethodFunc _ f _ = errIO $ runVoidIO f
    mkMethodTypes = Tagged $ MethodTypeInfo [] tyVoid

class IsVoidIO a
instance (IsVoidIO b) => IsVoidIO (a -> b)
instance IsVoidIO VoidIO

mkSpecialFunc :: forall tt ms.
    (Marshal tt, CanGetFrom tt ~ Yes, IsObjType tt ~ Yes,
        MethodSuffix ms, IsVoidIO ms) => (tt -> ms) -> UniformFunc
mkSpecialFunc f = \pt pv -> do
    hndl <- hsqmlGetObjectFromPointer pt
    this <- mFromHndl hndl
    runErrIO $ mkMethodFunc 0 (f this) pv

-- | Defines a named method using a function @f@ in the IO monad.
--
-- The first argument to @f@ receives the \"this\" object and hence must match
-- the type of the class on which the method is being defined. Subsequently,
-- there may be zero or more parameter arguments followed by an optional return
-- argument in the IO monad.
defMethod :: forall tt ms.
  (Marshal tt, CanGetFrom tt ~ Yes, IsObjType tt ~ Yes, MethodSuffix ms) =>
  String -> (tt -> ms) -> Member (GetObjType tt)
defMethod name f =
  let crude = untag (mkMethodTypes :: Tagged ms MethodTypeInfo)
  in Member MethodMember
       name
       (methodReturnType crude)
       (methodParamTypes crude)
       (mkUniformFunc f)
       Nothing
       Nothing

-- | Alias of 'defMethod' which is less polymorphic to reduce the need for type
-- signatures.
defMethod' :: forall obj ms. (Typeable obj, MethodSuffix ms) =>
    String -> (ObjRef obj -> ms) -> Member obj
defMethod' = defMethod

--
-- Signal
--

data SignalTypeInfo = SignalTypeInfo {
  signalParamTypes :: [TypeId]
}

-- | Defines a named signal. The signal is identified in subsequent calls to
-- 'fireSignal' using a 'SignalKeyValue'. This can be either i) type-based
-- using 'Proxy' @sk@ where @sk@ is an instance of the 'SignalKeyClass' class
-- or ii) value-based using a 'SignalKey' value creating using 'newSignalKey'.
defSignal ::
    forall obj skv. (SignalKeyValue skv) => String -> skv -> Member obj
defSignal name key =
    let crude = untag (mkSignalTypes ::
            Tagged (SignalValueParams skv) SignalTypeInfo)
    in Member SignalMember
        name
        tyVoid
        (signalParamTypes crude)
        (\_ _ -> return ())
        Nothing
        (Just $ signalKey key)

-- | Fires a signal defined on an object instance. The signal is identified
-- using either a type- or value-based signal key, as described in the
-- documentation for 'defSignal'. The first argument is the signal key, the
-- second is the object, and the remaining arguments, if any, are the arguments
-- to the signal as specified by the signal key.
--
-- If this function is called using a signal key which doesn't match a signal
-- defined on the supplied object, it will silently do nothing.
--
-- This function is safe to call from any thread. Any attached signal handlers
-- will be executed asynchronously on the event loop thread.
fireSignal ::
    forall tt skv. (Marshal tt, CanPassTo tt ~ Yes, IsObjType tt ~ Yes,
        SignalKeyValue skv) => skv -> tt -> SignalValueParams skv
fireSignal key this =
    let start cnt = postJob $ do
           hndl <- mToHndl this
           info <- hsqmlObjectGetHsTyperep hndl
           let slotMay = Map.lookup (signalKey key) $ cinfoSignals info
           case slotMay of
                Just slotIdx ->
                    withActiveObject hndl $ cnt $ SignalData hndl slotIdx
                Nothing ->
                    return () -- Should warn?
        cont ps (SignalData hndl slotIdx) =
            withArray (nullPtr:ps) (\pptr ->
                hsqmlFireSignal hndl slotIdx pptr)
    in mkSignalArgs start cont

data SignalData = SignalData HsQMLObjectHandle Int

-- | Values of the type 'SignalKey' identify distinct signals by value. The
-- type parameter @p@ specifies the signal's signature.
newtype SignalKey p = SignalKey Unique

-- | Creates a new 'SignalKey'. 
newSignalKey :: (SignalSuffix p) => IO (SignalKey p)
newSignalKey = fmap SignalKey $ newUnique

-- | Instances of the 'SignalKeyClass' class identify distinct signals by type.
-- The associated 'SignalParams' type specifies the signal's signature.
class (SignalSuffix (SignalParams sk)) => SignalKeyClass sk where
    type SignalParams sk

class (SignalSuffix (SignalValueParams skv)) => SignalKeyValue skv where
    type SignalValueParams skv
    signalKey :: skv -> MemberKey

instance (SignalKeyClass sk, Typeable sk) => SignalKeyValue (Proxy sk) where
    type SignalValueParams (Proxy sk) = SignalParams sk
    signalKey _ = TypeKey $ typeOf (undefined :: sk)

instance (SignalSuffix p) => SignalKeyValue (SignalKey p) where
    type SignalValueParams (SignalKey p) = p
    signalKey (SignalKey u) = DataKey u

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

--
-- Property
--

-- | Defines a named constant property using an accessor function in the IO
-- monad.
defPropertyConst :: forall tt tr.
    (Marshal tt, CanGetFrom tt ~ Yes, IsObjType tt ~ Yes, Marshal tr,
        CanReturnTo tr ~ Yes) => String ->
    (tt -> IO tr) -> Member (GetObjType tt)
defPropertyConst name g = Member ConstPropertyMember
    name
    (untag (mTypeCVal :: Tagged tr TypeId))
    []
    (mkUniformFunc g)
    Nothing
    Nothing

-- | Defines a named read-only property using an accessor function in the IO
-- monad.
defPropertyRO :: forall tt tr.
    (Marshal tt, CanGetFrom tt ~ Yes, IsObjType tt ~ Yes, Marshal tr,
        CanReturnTo tr ~ Yes) => String ->
    (tt -> IO tr) -> Member (GetObjType tt)
defPropertyRO name g = Member PropertyMember
    name
    (untag (mTypeCVal :: Tagged tr TypeId))
    []
    (mkUniformFunc g)
    Nothing
    Nothing

-- | Defines a named read-only property with an associated signal.
defPropertySigRO :: forall tt tr skv.
    (Marshal tt, CanGetFrom tt ~ Yes, IsObjType tt ~ Yes, Marshal tr,
        CanReturnTo tr ~ Yes, SignalKeyValue skv) => String -> skv ->
    (tt -> IO tr) -> Member (GetObjType tt)
defPropertySigRO name key g = Member PropertyMember
    name
    (untag (mTypeCVal :: Tagged tr TypeId))
    []
    (mkUniformFunc g)
    Nothing
    (Just $ signalKey key)

-- | Defines a named read-write property using a pair of accessor and mutator
-- functions in the IO monad.
defPropertyRW :: forall tt tr.
    (Marshal tt, CanGetFrom tt ~ Yes, IsObjType tt ~ Yes, Marshal tr,
        CanReturnTo tr ~ Yes, CanGetFrom tr ~ Yes) => String ->
    (tt -> IO tr) -> (tt -> tr -> IO ()) -> Member (GetObjType tt)
defPropertyRW name g s = Member PropertyMember
    name
    (untag (mTypeCVal :: Tagged tr TypeId))
    []
    (mkUniformFunc g)
    (Just $ mkSpecialFunc (\a b -> VoidIO $ s a b))
    Nothing

-- | Defines a named read-write property with an associated signal.
defPropertySigRW :: forall tt tr skv.
    (Marshal tt, CanGetFrom tt ~ Yes, IsObjType tt ~ Yes, Marshal tr,
        CanReturnTo tr ~ Yes, CanGetFrom tr ~ Yes, SignalKeyValue skv) =>
    String -> skv -> (tt -> IO tr) -> (tt -> tr -> IO ()) ->
    Member (GetObjType tt)
defPropertySigRW name key g s = Member PropertyMember
    name
    (untag (mTypeCVal :: Tagged tr TypeId))
    []
    (mkUniformFunc g)
    (Just $ mkSpecialFunc (\a b -> VoidIO $ s a b))
    (Just $ signalKey key)

-- | Alias of 'defPropertyConst' which is less polymorphic to reduce the need
-- for type signatures.
defPropertyConst' :: forall obj tr.
    (Typeable obj, Marshal tr, CanReturnTo tr ~ Yes) =>
    String -> (ObjRef obj -> IO tr) -> Member obj
defPropertyConst' = defPropertyConst

-- | Alias of 'defPropertyRO' which is less polymorphic to reduce the need for
-- type signatures.
defPropertyRO' :: forall obj tr.
    (Typeable obj, Marshal tr, CanReturnTo tr ~ Yes) =>
    String -> (ObjRef obj -> IO tr) -> Member obj
defPropertyRO' = defPropertyRO

-- | Alias of 'defPropertySigRO' which is less polymorphic to reduce the need
-- for type signatures.
defPropertySigRO' :: forall obj tr skv.
    (Typeable obj, Marshal tr, CanReturnTo tr ~ Yes, SignalKeyValue skv) =>
    String -> skv -> (ObjRef obj -> IO tr) -> Member obj
defPropertySigRO' = defPropertySigRO

-- | Alias of 'defPropertyRW' which is less polymorphic to reduce the need for
-- type signatures.
defPropertyRW' :: forall obj tr.
    (Typeable obj, Marshal tr, CanReturnTo tr ~ Yes, CanGetFrom tr ~ Yes) =>
    String -> (ObjRef obj -> IO tr) -> (ObjRef obj -> tr -> IO ()) -> Member obj
defPropertyRW' = defPropertyRW

-- | Alias of 'defPropertySigRW' which is less polymorphic to reduce the need
-- for type signatures.
defPropertySigRW' :: forall obj tr skv.
    (Typeable obj, Marshal tr, CanReturnTo tr ~ Yes, CanGetFrom tr ~ Yes,
        SignalKeyValue skv) => String -> skv ->
    (ObjRef obj -> IO tr) -> (ObjRef obj -> tr -> IO ()) -> Member obj
defPropertySigRW' = defPropertySigRW
