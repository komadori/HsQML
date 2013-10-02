{-# LANGUAGE ExistentialQuantification,
             ScopedTypeVariables,
             FlexibleInstances,
             DeriveDataTypeable,
             TypeFamilies #-}

module Graphics.QML.Test.Framework where

import Graphics.QML.Objects
import Graphics.QML.Marshal
import Graphics.QML.Test.MayGen

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Data.List (mapAccumR)
import Data.Maybe
import Data.Proxy
import Data.Typeable
import Data.IORef
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI

data TestType = forall a. (TestAction a) => TestType (Proxy a)

instance Eq TestType where
    (==) (TestType a) (TestType b) = typeOf a == typeOf b

instance Show TestType where
    showsPrec d (TestType p) =
        showParen (d > 10) $ showString "TestType (Proxy :: " .
            shows (typeOf p) . showString ")"

newtype Serial = Serial Int deriving (Show, Eq)

badSerial :: Serial
badSerial = Serial (-1)

data TestEnv = TestEnv {
    envSerial :: Serial,
    envHs     :: IntMap (TestType, Serial),
    envJs     :: IntMap (TestType, Serial)
} deriving Show

newTestEnv :: TestType -> TestEnv
newTestEnv tt =
    TestEnv (Serial 1) IntMap.empty (IntMap.singleton 0 (tt, Serial 0))

testEnvStep :: TestEnv -> TestEnv
testEnvStep (TestEnv (Serial s) h j) =
    TestEnv (Serial $ s+1) h j

testEnvSerial :: (Serial -> TestEnv -> TestEnv) -> TestEnv -> TestEnv
testEnvSerial f env = f (envSerial env) env

testEnvSetJ :: Int -> TestType -> Serial -> TestEnv -> TestEnv
testEnvSetJ n tt js (TestEnv s h j) =
    TestEnv s h (IntMap.insert n (tt,js) j)

testEnvIsaJ :: Int -> TestType -> TestEnv -> Bool
testEnvIsaJ n tt env =
    case IntMap.lookup n $ envJs env of
        Just (tt', _) -> tt' == tt
        _             -> False

testEnvNextJ :: TestEnv -> Int
testEnvNextJ = (+1) . fst . IntMap.findMax . envJs

testEnvListJ :: TestType -> TestEnv -> [Int]
testEnvListJ tt env =
    map fst $ filter (\(_,(tt',_)) ->
        tt' == tt) $ IntMap.toList $ envJs env

data TestBox = forall a. (TestAction a) => TestBox Int a

instance Show TestBox where
    showsPrec d (TestBox n a) =
        showParen (d > 10) $ showString "TestBox " . shows n .
            showString " " . showsPrec 11 a

testBoxType :: TestBox -> TestType
testBoxType (TestBox _ a) =
    TestType $ mkProxy a
    where mkProxy = const Proxy :: a -> Proxy a

class (Typeable a, Show a) => TestAction a where
    legalActionIn  :: a -> TestEnv -> Bool
    nextActionsFor :: TestEnv -> MayGen a
    updateEnvRaw   :: a -> TestEnv -> TestEnv
    actionRemote   :: a -> Int -> (ShowS, ShowS)
    mockObjDef     :: [Member (MockObj a)]

legalAction :: TestEnv -> TestBox -> Bool
legalAction env tb@(TestBox n a) =
    (fst $ fromJust $ IntMap.lookup n $ envJs env) == testBoxType tb &&
        legalActionIn a env

nextActions :: TestEnv -> Gen TestBox
nextActions env =
    oneof $ mapMaybe (uncurry f) $ (:[]) $ head $ IntMap.toList $ envJs env
    where f n ((TestType pxy), _) =
              (mayGen $ fmap (TestBox n . flip asProxyTypeOf pxy) $
                  nextActionsFor env)

updateEnv :: TestBox -> TestEnv -> TestEnv
updateEnv (TestBox _ a) = updateEnvRaw a

showTestCode :: [TestBox] -> ShowS
showTestCode xs =
    uncurry (.) $ foldr (\(a,b) (c,d) -> (a . c, b . d)) (id,id) $ map f xs
    where f (TestBox n a) = actionRemote a n

newtype TestBoxSrc a = TestBoxSrc { srcTestBoxes :: [TestBox]} deriving Show

genTestBoxes :: Int -> TestEnv -> Gen [TestBox]
genTestBoxes 0 _ = return []
genTestBoxes len env = do
    x <- nextActions env
    xs <- genTestBoxes (len-1) (updateEnv x env)
    return (x:xs)

partitions :: [a] -> [([a],[a])]
partitions xs =
    ([],xs) : case xs of
        []    -> []
        x:xs' -> map (\(hs,ts) -> (x:hs,ts)) $ partitions xs'

partitions1 :: [a] -> [([a],a,[a])]
partitions1 xs =
    mapMaybe f $ partitions xs
    where f (_,[]) = Nothing
          f (as,(b:cs)) = Just (as,b,cs)

pruneTestBoxes :: TestEnv -> [TestBox] -> [TestBox]
pruneTestBoxes env xs =
    catMaybes $ snd $ mapAccumR f env xs
    where f e x | legalAction e x = (updateEnv x e, Just x)
                | otherwise       = (e, Nothing)

shrinkHelper :: TestEnv -> [TestBox] -> [TestBox] -> [TestBox]
shrinkHelper env xs ys =
    let env' = foldr updateEnv env xs
        ys'  = pruneTestBoxes env' ys
    in xs ++ ys'

instance (TestAction a) => Arbitrary (TestBoxSrc a) where
    arbitrary =
        fmap TestBoxSrc $ sized $ \sz ->
            genTestBoxes sz $ newTestEnv $ TestType (Proxy :: Proxy a)
    shrink (TestBoxSrc xs) =
        map (\(a,_,b) -> TestBoxSrc $ shrinkHelper env a b) $ partitions1 xs
        where env = newTestEnv $ TestType (Proxy :: Proxy a)

mockFromSrc :: forall a. (TestAction a) => TestBoxSrc a -> IO (MockObj a)
mockFromSrc (TestBoxSrc ts) = do 
    statusRef <- newIORef $ TestStatus ts Nothing
        (newTestEnv $ TestType (Proxy :: Proxy a)) IntMap.empty
    return $ MockObj (Serial 0) statusRef

data TestFault
    = TExcessAction
    | TBadAction
    | TBadActionType
    | TBadActionCtor
    | TBadActionSlot
    | TBadActionData
    | TTimeout
    | TInvalid
    deriving Show

newtype Any = Any () deriving Show

data TestStatus = TestStatus {
    testList  :: [TestBox],
    testFault :: Maybe TestFault,
    testEnv   :: TestEnv,
    testObjs  :: IntMap Any
} deriving Show

testSerial :: TestStatus -> Serial
testSerial = envSerial . testEnv

data MockObj a = MockObj {
    mockSerial :: Serial,
    mockStatus :: IORef TestStatus
} deriving Typeable

mockGetStatus :: MockObj a -> IO TestStatus
mockGetStatus (MockObj _ statusRef) = readIORef statusRef

class MakeDefault a where
    makeDef :: IO a

instance (TestAction a) => MakeDefault (ObjRef (MockObj a)) where
    makeDef = do
        statusRef <- newIORef $ TestStatus [] (Just TInvalid)
            (TestEnv badSerial IntMap.empty IntMap.empty) IntMap.empty
        newObject $ MockObj badSerial statusRef

instance MakeDefault () where
    makeDef = return ()

instance MakeDefault Int32 where
    makeDef = return 0

instance MakeDefault Double where
    makeDef = return (0/0)

instance MakeDefault [a] where
    makeDef = return []

instance MakeDefault Text where
    makeDef = return T.empty

instance MakeDefault URI where
    makeDef = return $ URI "" Nothing "" "" ""

expectAction :: (TestAction a, MakeDefault b) =>
    MockObj a -> (a -> IO (Either TestFault b)) -> IO b
expectAction mock pred = do
    status <- readIORef $ mockStatus mock
    res <- case status of
        TestStatus (b:_) Nothing env _ -> case b of
            TestBox _ a -> case cast a of
                Just a' -> pred a' >>= return . fmap ((,) (updateEnvRaw a' env))
                _       -> return $ Left TBadActionType
        TestStatus [] Nothing _ _       -> return $ Left TExcessAction
        TestStatus _ (Just f) _ _       -> return $ Left f
    case res of
        Left f  -> do
            let (TestStatus bs _ env objs) = status
            writeIORef (mockStatus mock) $ TestStatus bs (Just f) env objs
            makeDef
        Right (env', v) -> do
            let (TestStatus (_:bs) _ _ objs) = status
            writeIORef (mockStatus mock) $ TestStatus bs Nothing env' objs
            return v

badAction :: (MakeDefault b) => MockObj a -> IO b
badAction mock = do
    status <- readIORef $ mockStatus mock
    writeIORef (mockStatus mock) $ status {testFault = Just TBadAction} 
    makeDef

instance (TestAction a) => Object (MockObj a) where
    classDef = defClass mockObjDef

instance (TestAction a) => Marshal (MockObj a) where
    type MarshalMode (MockObj a) = ValObjToOnly (MockObj a)
    marshaller = objSimpleMarshaller
