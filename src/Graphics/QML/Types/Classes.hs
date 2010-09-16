{-# LANGUAGE
    RankNTypes,
    ScopedTypeVariables,
    FlexibleInstances,
    UndecidableInstances,
    OverlappingInstances
  #-}

-- | Facilities for defining new object types which can be marshalled between
-- Haskell and QML.
module Graphics.QML.Types.Classes (
  -- * Classes
  MetaClass,
  mkClass,
  MetaObject(
    metaClass),
  MetaMember,

  -- * Methods
  mkMethod0,
  mkMethod1,
  mkMethod2,
  mkMethod3,

  -- * Properties
  mkPropertyRO,
  mkPropertyRW,
) where

import Graphics.QML.Internal.Core
import Graphics.QML.Internal.Classes

import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import Foreign.Marshal.Array
import System.IO.Unsafe

--
-- MetaClass
--

-- | Represents a QML class which wraps the type @a@.
data MetaClass a = MetaClass {
  classType :: TypeName,
  classData :: HsQMLClassHandle
}

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave (x:xs) ys = x : ys `interleave` xs 

mkClassIO :: (Marshallable a) =>
  String -> [Method a] -> [Property a] -> IO (MetaClass a)
mkClassIO name methods properties = do
  let (MOCOutput metaData metaStrData) = compileClass name methods properties
  metaDataPtr <- newArray metaData
  metaStrDataPtr <- newArray metaStrData
  methodsPtr <- mapM (marshalFunc . methodFunc) methods >>= newArray
  pReads <- mapM (marshalFunc . propertyReadFunc) properties
  pWrites <- mapM (fromMaybe (return nullFunPtr) . fmap marshalFunc .
    propertyWriteFunc) properties
  propertiesPtr <- newArray $ interleave pReads pWrites
  hndl <- hsqmlCreateClass metaDataPtr metaStrDataPtr methodsPtr propertiesPtr
  case hndl of 
    Just hndl' -> return (MetaClass (TypeName name) hndl')
    Nothing    -> mkClassIO (name++"_1") methods properties

-- | Creates a 'MetaClass' given a description of its methods and properties.
mkClass :: (Marshallable a) =>
  String -> [MetaMember a] -> MetaClass a
mkClass name members =
  unsafePerformIO $ mkClassIO name methods properties
  where methods = mapMaybe toMethod members
        properties = mapMaybe toProperty members


-- | The class 'MetaObject' allows Haskell types to be accessed as objects
-- from within QML. A 'Marshallable' instance is provided automatically
-- for all instances of this class.
class MetaObject a where
  -- | Gets the 'MetaClass' corresponding to the 'MetaObject' type @a@.
  metaClass :: MetaClass a

instance (MetaObject a) => Marshallable a where
  marshal ptr obj = do
    (HsQMLObjectHandle hndl) <-
      hsqmlCreateObject obj $ classData $ (metaClass :: MetaClass a)
    poke (castPtr ptr) hndl
  unmarshal ptr =
    hsqmlGetHaskell $ HsQMLObjectHandle $ castPtr ptr
  mSizeOf _ = sizeOf nullPtr
  mTypeOf _ = classType (metaClass :: MetaClass a) 

--
-- MetaMember
--

-- | Represents a member of the QML class which wraps the type @a@.
data MetaMember a
  = MethodMember (Method a)
  | PropertyMember (Property a)

toMethod :: MetaMember a -> Maybe (Method a)
toMethod (MethodMember m) = Just m
toMethod _                = Nothing

toProperty :: MetaMember a -> Maybe (Property a)
toProperty (PropertyMember p) = Just p
toProperty _                  = Nothing

--
-- Method
--

-- | Represents a named method which can be invoked from QML on an object of
-- type @a@.
data Method a = Method {
  -- | Gets the name of a 'Method'.
  methodName  :: String,
  -- | Gets the 'TypeName's which comprise the signature of a 'Method'.
  -- The head of the list is the return type and the tail the arguments.
  methodTypes :: [TypeName],
  methodFunc  :: UniformFunc
}

-- | Creates a 'MetaMember' for a named impure nullary function.
mkMethod0 ::
  forall a tr. (Marshallable a, Marshallable tr) =>
  String -> (a -> IO tr) -> MetaMember a
mkMethod0 name f = MethodMember $ Method name
  [mTypeOf (undefined :: tr)]
  (marshalFunc0 $ \p0 pr -> unmarshal p0 >>= f >>= marshalRet pr)

-- | Creates a 'MetaMember' for a named impure unary function.
mkMethod1 ::
  forall a t1 tr. (Marshallable a, Marshallable t1, Marshallable tr) =>
  String -> (a -> t1 -> IO tr) -> MetaMember a
mkMethod1 name f = MethodMember $ Method name
  [mTypeOf (undefined :: tr), mTypeOf (undefined :: t1)]
  (marshalFunc1 $ \p0 p1 pr -> do
    v0 <- unmarshal p0
    v1 <- unmarshal p1
    f v0 v1 >>= marshalRet pr)

-- | Creates a 'MetaMember' for a named impure binary function.
mkMethod2 ::
  forall a t1 t2 tr.
  (Marshallable a, Marshallable t1, Marshallable t2, Marshallable tr) =>
  String -> (a -> t1 -> t2 -> IO tr) -> MetaMember a
mkMethod2 name f = MethodMember $ Method name
  [mTypeOf (undefined :: tr), mTypeOf (undefined :: t1),
   mTypeOf (undefined :: t2)]
  (marshalFunc2 $ \p0 p1 p2 pr -> do
    v0 <- unmarshal p0
    v1 <- unmarshal p1
    v2 <- unmarshal p2
    f v0 v1 v2 >>= marshalRet pr)

-- | Creates a 'MetaMember' for a named impure function which takes 3
-- arguments.
mkMethod3 ::
  forall a t1 t2 t3 tr.
  (Marshallable a, Marshallable t1, Marshallable t2, Marshallable t3,
   Marshallable tr) =>
  String -> (a -> t1 -> t2 -> t3 -> IO tr) -> MetaMember a
mkMethod3 name f = MethodMember $ Method name
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
-- of type @a@.
data Property a = Property {
  -- | Gets the name of a 'Property'.
  propertyName :: String,
  propertyType :: TypeName,
  propertyReadFunc :: UniformFunc,
  propertyWriteFunc :: Maybe UniformFunc
}

-- | Creates a 'MetaMember' for a named read-only property using an impure
-- accessor function.
mkPropertyRO ::
  forall a tr. (Marshallable a, Marshallable tr) =>
  String -> (a -> IO tr) -> MetaMember a
mkPropertyRO name g = PropertyMember $ Property name
  (mTypeOf (undefined :: tr))
  (marshalFunc0 $ \p0 pr -> unmarshal p0 >>= g >>= marshal pr)
  Nothing

-- | Creates a 'MetaMember' for a named read-write property using a pair of 
-- impure accessor and mutator functions.
mkPropertyRW ::
  forall a tr. (Marshallable a, Marshallable tr) =>
  String -> (a -> IO tr) -> (a -> tr -> IO ()) -> MetaMember a
mkPropertyRW name g s = PropertyMember $ Property name
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

marshalRet :: (Marshallable a) => Ptr () -> a -> IO ()
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

writeMethod :: Method a -> State MOCState ()
writeMethod m = do
  idx <- get >>= return . mDataLen
  writeString $ methodSignature m
  writeString $ methodParameters m
  writeString $ typeName $ head $ methodTypes m
  writeInt mfAccessPublic
  state <- get
  put $ state {mDataMethodsIdx = mplus (mDataMethodsIdx state) (Just idx)}
  return ()

writeProperty :: Property a -> State MOCState ()
writeProperty p = do
  idx <- get >>= return . mDataLen
  writeString $ propertyName p
  writeString $ typeName $ propertyType p
  writeInt 0 -- FIXME
  state <- get
  put $ state {mDataPropsIdx = mplus (mDataPropsIdx state) (Just idx)}
  return ()

compileClass :: String -> [Method a] -> [Property a] -> MOCOutput
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

methodSignature :: Method a -> String
methodSignature method =
  let paramTypes = tail $ methodTypes method
  in (showString (methodName method) . showChar '(' .
       foldr0 (\l r -> l . showChar ',' . r) id
         (map (showString . typeName) paramTypes) . showChar ')') ""

methodParameters :: Method a -> String
methodParameters method =
  replicate (flip (-) 2 $ length $ methodTypes method) ','
