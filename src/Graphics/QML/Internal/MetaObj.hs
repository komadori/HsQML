module Graphics.QML.Internal.MetaObj where

import Graphics.QML.Internal.Types

import Control.Monad
import Control.Monad.Trans.State (State, execState, get, put)
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

crlSingle :: a -> CRList a
crlSingle x = CRList 1 [x]

crlAppend1 :: CRList a -> a -> CRList a
crlAppend1 (CRList n xs) x = CRList (n+1) (x:xs)

crlAppend :: CRList a -> [a] -> CRList a
crlAppend (CRList n xs) ys = CRList n' xs'
  where (xs', n')       = rev ys xs n
        rev []     vs m = (vs, m)
        rev (u:us) vs m = rev us (u:vs) (m+1)

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

crlToList :: CRList a -> [a]
crlToList (CRList _ lst) = reverse lst

--
-- Meta Object Compiler
--

data MemberKind
    = MethodMember
    | ConstPropertyMember
    | PropertyMember
    | SignalMember
    deriving Eq

-- | Represents a named member of the QML class which wraps type @tt@.
data Member tt = Member {
    memberKind   :: MemberKind,
    memberName   :: String,
    memberType   :: TypeId,
    memberParams :: [TypeId],
    memberFun    :: UniformFunc,
    memberFunAux :: Maybe UniformFunc,
    memberKey    :: Maybe MemberKey
}

data MOCState = MOCState {
  mData            :: CRList CUInt,
  mDataMethodsIdx  :: Maybe Int,
  mDataPropsIdx    :: Maybe Int,
  mStrChar         :: CRList CChar,
  mStrInfo         :: CRList CUInt,
  mStrMap          :: Map String CUInt,
  mParamMap        :: Map [TypeId] CUInt,
  mSigMap          :: Map MemberKey CUInt,
  mFuncMethods     :: CRList (Maybe UniformFunc),
  mFuncProperties  :: CRList (Maybe UniformFunc),
  mMethodCount     :: Int,
  mSignalCount     :: Int,
  mPropertyCount   :: Int
}

-- | Generate MOC meta-data from a class name and member list.
compileClass :: String -> [Member tt] -> MOCState
compileClass name ms = 
  let enc = flip execState (newMOCState enc) $ do
        writeInt 7                           -- Revision
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
        let mms = filterMembers SignalMember ms ++
                  filterMembers MethodMember ms
        mapM_ writeMethodParams mms
        mapM_ writeMethod mms
        let pms = filterMembers ConstPropertyMember ms ++
                  filterMembers PropertyMember ms
        mapM_ writeProperty pms
        mapM_ writePropertySig pms
        writeInt 0
  in enc

filterMembers :: MemberKind -> [Member tt] -> [Member tt]
filterMembers k = filter (\m -> k == memberKind m)

newMOCState :: MOCState -> MOCState
newMOCState enc = MOCState
    crlEmpty Nothing Nothing crlEmpty (crlSingle strCount) Map.empty
    Map.empty Map.empty crlEmpty crlEmpty 0 0 0
    where strCount = fromIntegral $ Map.size $ mStrMap enc
 
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
  let msChr = mStrChar state
      msInf = mStrInfo state
      msMap = mStrMap state
  case Map.lookup str msMap of
    Just idx -> writeInt idx
    Nothing  -> do
      let idx = crlLen msInf - 1
          msChr' = msChr `crlAppend` map castCharToCChar str `crlAppend1` 0
          msInf' = msInf `crlAppend1` fromIntegral (crlLen msChr')
          msMap' = Map.insert str (fromIntegral idx) msMap
      put $ state {
        mStrChar = msChr',
        mStrInfo = msInf',
        mStrMap = msMap'}
      writeIntegral idx

writeMethodParams :: Member tt -> State MOCState ()
writeMethodParams m = do
  state <- get
  let types = memberTypes m
      datal = mData state
      mpMap = mParamMap state
  case Map.lookup types mpMap of
    Just _ -> return ()
    Nothing  -> do
      let idx = crlLen datal
          mpMap' = Map.insert types (fromIntegral idx) mpMap
      put $ state {
        mParamMap = mpMap'}
      mapM_ (writeInt . typeId) types
      replicateM_ (length $ memberParams m) $ writeString ""

writeMethod :: Member tt -> State MOCState ()
writeMethod m = do
  idx <- get >>= return . crlLen . mData
  paramMap <- get >>= return . mParamMap
  writeString $ memberName m
  writeIntegral $ length $ memberParams m
  writeInt $ fromMaybe 0 $ flip Map.lookup paramMap $ memberTypes m
  writeString ""
  let (mc,sc,flags) = case memberKind m of
        SignalMember -> (0,1,mfMethodSignal)
        _            -> (1,0,mfMethodMethod)
  writeInt (mfAccessPublic .|. mfMethodScriptable .|. flags)
  state <- get
  put $ state {
    mDataMethodsIdx = mplus (mDataMethodsIdx state) (Just idx),
    mMethodCount = mc + mMethodCount state,
    mSignalCount = sc + mSignalCount state,
    mSigMap = maybe (mSigMap state) (\k ->
      Map.insert k (fromIntegral $ mSignalCount state) (mSigMap state)) $
      memberKey m,
    mFuncMethods = mFuncMethods state `crlAppend1` (Just $ memberFun m)}
  return ()

writeProperty :: Member tt -> State MOCState ()
writeProperty p = do
  idx <- get >>= return . crlLen . mData
  writeString $ memberName p
  writeInt $ typeId $ memberType p
  writeInt (pfReadable .|. pfScriptable .|.
    (if ConstPropertyMember == memberKind p then pfConstant else 0) .|.
    (if isJust (memberFunAux p) then pfWritable else 0) .|.
    (if isJust (memberKey p) then pfNotify else 0))
  state <- get
  put $ state {
    mDataPropsIdx = mplus (mDataPropsIdx state) (Just idx),
    mPropertyCount = 1 + mPropertyCount state,
    mFuncProperties = mFuncProperties state
      `crlAppend1` (Just $ memberFun p) `crlAppend1` memberFunAux p
  }
  return ()

writePropertySig :: Member tt -> State MOCState ()
writePropertySig p = do
  state <- get
  writeInt $ fromMaybe 0 $ maybe Nothing (flip Map.lookup $ mSigMap state) $
    memberKey p

memberTypes :: Member tt -> [TypeId]
memberTypes m = memberType m : memberParams m

typeId :: TypeId -> CUInt
typeId (TypeId tyid) = fromIntegral tyid

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
