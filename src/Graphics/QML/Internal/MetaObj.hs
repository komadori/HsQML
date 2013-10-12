module Graphics.QML.Internal.MetaObj where

import Graphics.QML.Internal.BindObj
import Graphics.QML.Internal.Marshal
import Graphics.QML.Internal.Objects

import Control.Monad
import Control.Monad.Trans.State
import Data.Bits
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array

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

filterMembers :: MemberKind -> [Member tt] -> [Member tt]
filterMembers k ms =
  filter (\m -> k == memberKind m) ms

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

--
-- Constants
--

ofDynamicMetaObject :: CUInt
ofDynamicMetaObject = 0x01

mfAccessPrivate, mfAccessProtected, mfAccessPublic, mfAccessMask,
  mfMethodMethod, mfMethodSignal, mfMethodSlot, mfMethodConstructor,
  mfMethodTypeMask, mfMethodCompatibility, mfMethodCloned, mfMethodScriptable
  :: CUInt
mfAccessPrivate   = 0x00
mfAccessProtected = 0x01
mfAccessPublic    = 0x02
mfAccessMask      = 0x03
mfMethodMethod      = 0x00
mfMethodSignal      = 0x04
mfMethodSlot        = 0x08
mfMethodConstructor = 0x0c
mfMethodTypeMask    = 0x0c
mfMethodCompatibility = 0x10
mfMethodCloned        = 0x20
mfMethodScriptable    = 0x40

pfInvalid, pfReadable, pfWritable, pfResettable, pfEnumOrFlag, pfStdCppSet,
  pfConstant, pfFinal, pfDesignable, pfResolveDesignable, pfScriptable,
  pfResolveScriptable, pfStored, pfResolveStored, pfEditable,
  pfResolveEditable, pfUser, pfResolveUser, pfNotify :: CUInt
pfInvalid           = 0x00000000
pfReadable          = 0x00000001
pfWritable          = 0x00000002
pfResettable        = 0x00000004
pfEnumOrFlag        = 0x00000008
pfStdCppSet         = 0x00000100
pfConstant          = 0x00000400
pfFinal             = 0x00000800
pfDesignable        = 0x00001000
pfResolveDesignable = 0x00002000
pfScriptable        = 0x00004000
pfResolveScriptable = 0x00008000
pfStored            = 0x00010000
pfResolveStored     = 0x00020000
pfEditable          = 0x00040000
pfResolveEditable   = 0x00080000
pfUser              = 0x00100000
pfResolveUser       = 0x00200000
pfNotify            = 0x00400000
