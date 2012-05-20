{-# LANGUAGE
    ScopedTypeVariables,
    TypeFamilies,
    FlexibleInstances,
    UndecidableInstances,
    OverlappingInstances
  #-}

-- | Facilities for defining new object types which can be marshalled between
-- Haskell and QML.
module Graphics.QML.Types.Classes (
  -- * Classes
  MetaObject (
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
) where

import Graphics.QML.Internal.Core
import Graphics.QML.Internal.Classes

import Control.Monad
import Control.Monad.Trans.State
import Data.Bits
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import Foreign.Marshal.Array
import System.IO.Unsafe
import Numeric

--
-- MetaObject
--

-- | The class 'MetaObject' allows Haskell types to be accessed as objects
-- from within QML.
--
-- A 'Marshallable' instance is provided automatically for all instances of
-- this class, however, 'defClass' must be used to define an object's class
-- members before marshalling any values.
{-# NOINLINE classDef #-}
class (Typeable tt) => MetaObject tt where
  classDef :: ClassDef tt

instance (MetaObject tt) => Marshallable tt where
  marshal ptr obj = do
    (HsQMLObjectHandle hndl) <-
      hsqmlCreateObject obj $ classHndl $ (classDef :: ClassDef tt)
    poke (castPtr ptr) hndl
  unmarshal ptr =
    hsqmlGetHaskell $ HsQMLObjectHandle $ castPtr ptr
  mSizeOf _ = sizeOf nullPtr
  mTypeOf _ = classType (classDef :: ClassDef tt)

--
-- ClassDef
--

-- | Represents a QML class which wraps the type @tt@.
data ClassDef tt = ClassDef {
  classType :: TypeName,
  classHndl :: HsQMLClassHandle
}

-- | Generates a 'ClassDef' from a list of 'Member's.
defClass :: forall tt. (MetaObject tt) => [Member tt] -> ClassDef tt
defClass ms = unsafePerformIO $ do
  let typ  = typeOf (undefined :: tt)
      name = tyConString $ typeRepTyCon typ
  key <- typeRepKey typ
  createClass (name ++ showInt key "") ms

createClass :: forall tt. (MetaObject tt) =>
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
  forall tt tr. (MetaObject tt, Marshallable tr) =>
  String -> (tt -> IO tr) -> Member tt
defMethod0 name f = MethodMember $ Method name
  [mTypeOf (undefined :: tr)]
  (marshalFunc0 $ \p0 pr -> unmarshal p0 >>= f >>= marshalRet pr)

-- | Defines a named method using an impure unary function.
defMethod1 ::
  forall tt t1 tr. (MetaObject tt, Marshallable t1, Marshallable tr) =>
  String -> (tt -> t1 -> IO tr) -> Member tt
defMethod1 name f = MethodMember $ Method name
  [mTypeOf (undefined :: tr), mTypeOf (undefined :: t1)]
  (marshalFunc1 $ \p0 p1 pr -> do
    v0 <- unmarshal p0
    v1 <- unmarshal p1
    f v0 v1 >>= marshalRet pr)

-- | Defines a named method using an impure binary function.
defMethod2 ::
  forall tt t1 t2 tr.
  (MetaObject tt, Marshallable t1, Marshallable t2, Marshallable tr) =>
  String -> (tt -> t1 -> t2 -> IO tr) -> Member tt
defMethod2 name f = MethodMember $ Method name
  [mTypeOf (undefined :: tr), mTypeOf (undefined :: t1),
   mTypeOf (undefined :: t2)]
  (marshalFunc2 $ \p0 p1 p2 pr -> do
    v0 <- unmarshal p0
    v1 <- unmarshal p1
    v2 <- unmarshal p2
    f v0 v1 v2 >>= marshalRet pr)

-- | Defines a named method using an impure function taking 3 arguments.
defMethod3 ::
  forall tt t1 t2 t3 tr.
  (MetaObject tt, Marshallable t1, Marshallable t2, Marshallable t3,
   Marshallable tr) =>
  String -> (tt -> t1 -> t2 -> t3 -> IO tr) -> Member tt
defMethod3 name f = MethodMember $ Method name
  [mTypeOf (undefined :: tr), mTypeOf (undefined :: t1),
   mTypeOf (undefined :: t2), mTypeOf (undefined :: t3)]
  (marshalFunc3 $ \p0 p1 p2 p3 pr -> do
    v0 <- unmarshal p0
    v1 <- unmarshal p1
    v2 <- unmarshal p2
    v3 <- unmarshal p3
    f v0 v1 v2 v3 >>= marshalRet pr)

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
  forall tt tr. (MetaObject tt, Marshallable tr) =>
  String -> (tt -> IO tr) -> Member tt
defPropertyRO name g = PropertyMember $ Property name
  (mTypeOf (undefined :: tr))
  (marshalFunc0 $ \p0 pr -> unmarshal p0 >>= g >>= marshal pr)
  Nothing

-- | Defines a named read-write property using a pair of 
-- impure accessor and mutator functions.
defPropertyRW ::
  forall tt tr. (MetaObject tt, Marshallable tr) =>
  String -> (tt -> IO tr) -> (tt -> tr -> IO ()) -> Member tt
defPropertyRW name g s = PropertyMember $ Property name
  (mTypeOf (undefined :: tr))
  (marshalFunc0 $ \p0 pr -> unmarshal p0 >>= g >>= marshal pr)
  (Just $ marshalFunc1 $ \p0 p1 _ -> do
    v0 <- unmarshal p0
    v1 <- unmarshal p1
    s v0 v1)

--
-- ???
--

marshalFunc0 :: (Ptr () -> Ptr () -> IO ()) -> UniformFunc
marshalFunc0 f p0 pv = do
  pr <- peekElemOff pv 0
  f p0 pr

marshalFunc1 :: (Ptr () -> Ptr () -> Ptr () -> IO ()) -> UniformFunc
marshalFunc1 f p0 pv = do
  pr <- peekElemOff pv 0
  p1 <- peekElemOff pv 1
  f p0 p1 pr

marshalFunc2 ::
  (Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO ()) -> UniformFunc
marshalFunc2 f p0 pv = do
  pr <- peekElemOff pv 0
  p1 <- peekElemOff pv 1
  p2 <- peekElemOff pv 2
  f p0 p1 p2 pr

marshalFunc3 ::
  (Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO ()) -> UniformFunc
marshalFunc3 f p0 pv = do
  pr <- peekElemOff pv 0
  p1 <- peekElemOff pv 1
  p2 <- peekElemOff pv 2
  p3 <- peekElemOff pv 3
  f p0 p1 p2 p3 pr

marshalRet :: (Marshallable tt) => Ptr () -> tt -> IO ()
marshalRet ptr obj
  | ptr == nullPtr = return ()
  | otherwise      = marshal ptr obj

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
