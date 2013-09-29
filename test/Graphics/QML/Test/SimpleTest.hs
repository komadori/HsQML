{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}

module Graphics.QML.Test.SimpleTest where

import Graphics.QML.Objects
import Graphics.QML.Test.Framework

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Data.Typeable
import Data.Proxy
import Data.Char
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Numeric

import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI

showJDouble :: Double -> ShowS
showJDouble x | isNaN x                 = showString "(0/0)"
              | isInfinite x && (x < 0) = showString "(-1/0)"
              | isInfinite x            = showString "(1/0)"
              | isNegativeZero x        = showString "-0"
              | otherwise               = shows x

showJString :: String -> ShowS
showJString [] =
    showString "\"\""
showJString cs =
    showChar '"' . (foldr1 (.) $ map f cs) . showChar '"'
    where f '\"' = showString "\\\""
          f '\\' = showString "\\\\"
          f c | ord c < 32 = hexEsc c
              | ord c > 127 = hexEsc c
              | otherwise  = showChar c
          hexEsc c = let h = showHex (ord c)
                     in showString "\\u" .
                        showString (replicate (4 - (length $ h "")) '0') . h

showJURI :: URI -> ShowS
showJURI = showJString . ($ "") . uriToString id

showVar :: Int -> ShowS
showVar 0 = error "Cannot use context."
showVar n = showString "x" . shows n

showVarDecl :: Int -> ShowS
showVarDecl 0 = error "Cannot reassign context."
showVarDecl n = showString "var " . showVar n . showString " = "

showVarDot :: Int -> ShowS
showVarDot 0 = id
showVarDot n = showVar n . showChar '.'

noScope :: ShowS -> (ShowS, ShowS)
noScope s = (s, id)

showTest :: Int -> String -> ShowS -> (ShowS, ShowS)
showTest n name value = noScope $
    showString "if (" . showVarDot n . showString name .
    showString "() != " . value . showString ") {window.close();}\n"

showCall :: Int -> String -> ShowS -> (ShowS, ShowS)
showCall n name params = noScope $
    showVarDot n . showString name .
    showString "(" . params . showString ");\n"

capSize :: Int -> Gen a -> Gen a
capSize cap g = sized (\s -> if s > cap then resize cap g else resize s g)

uriGen :: Gen URI
uriGen = capSize 35 $ do
    let slists  = fmap (:[])
        listxyz = fmap concat . sequence
        listxs  = fmap concat . listOf
        listxs1 = fmap concat . listOf1
        lower   = elements $ slists $ enumFromTo 'a' 'z'
        upper   = elements $ slists $ enumFromTo 'A' 'Z'
        digit   = elements $ slists "01234567989"
        mark    = elements $ slists "-_.!~*'()"
        sextra  = elements $ slists "+-."
        rextra  = elements $ slists "$,;:@&=+"
        dash    = return "-"
        dot     = return "."
        alpha   = oneof [lower, upper]
        alphnum = oneof [lower, upper, digit]
        dchar   = oneof [lower, digit]
        dchar2  = frequency [(9,lower), (5,digit), (1,dash)]
        unres   = frequency [(9,alphnum), (1,mark)]
        escNum  = oneof [choose (0,31), choose (128,255)] :: Gen Int
        pad     = \n x -> replicate (n - length x) '0' ++ x
        escape  = fmap (('%':) . pad 2 . flip showHex "") escNum
        scheme  = listxyz [alpha, listxs $ frequency [
            (9,alphnum), (1,sextra)]]
        dpart1  = listxyz [
                      frequency [(9,dchar), (1,listxyz [dchar, dot, dchar])],
                      listxs $ frequency [
                          (9,dchar2), (1,listxyz [dchar, dot, dchar]),
                          (1,listxyz [dchar, dot, dchar, dot])],
                      dchar, dot]
        dpart2  = oneof [lower, listxyz [lower, listxs dchar2, dchar]]
        regName = flip suchThat (\x -> length x < 255) $ listxyz [
            frequency [(9,dpart1), (1,return "")],
            dpart2, oneof [dot, return ""]]
        segment = fmap ('/':) $ listxs $ frequency [
            (9,unres), (1,escape), (1,rextra)]
        path   = listxs1 segment
    schemeStr <- scheme
    regNameStr <- regName
    pathStr <- path
    return $
        URI (schemeStr++":") (Just $ URIAuth "" regNameStr "") pathStr "" ""

data SimpleMethods
    = SMTrivial
    | SMGetInt Int32
    | SMSetInt Int32
    | SMGetDouble Double
    | SMSetDouble Double
    | SMGetString String
    | SMSetString String
    | SMGetText Text
    | SMSetText Text
    | SMGetURI URI
    | SMSetURI URI
    | SMGetObject Int
    | SMSetObject Int
    deriving (Show, Typeable)

instance TestAction SimpleMethods where
    legalActionIn (SMSetObject n) env =
        case IntMap.lookup n $ envJs env of
            Just (tt, _) -> tt == testObjectType
            _            -> False
    legalActionIn _ _ = True
    nextActionsFor env =
        let baseGens = [
                return SMTrivial,
                arbitrary >>= return . SMGetInt,
                arbitrary >>= return . SMSetInt,
                arbitrary >>= return . SMGetDouble,
                arbitrary >>= return . SMSetDouble,
                arbitrary >>= return . SMGetString,
                arbitrary >>= return . SMSetString,
                arbitrary >>= return . SMGetText . T.pack,
                arbitrary >>= return . SMSetText . T.pack,
                uriGen >>= return . SMGetURI,
                uriGen >>= return . SMSetURI,
                return $ SMGetObject $ (+1) $ fst $ IntMap.findMax $ envJs env]
            availObjs = map fst $ filter (\(_,(tt,_)) -> tt == testObjectType) $
                IntMap.toList $ envJs env
            setObjGen = elements availObjs >>= return . SMSetObject
        in Just $ oneof $ if null availObjs
               then baseGens else setObjGen : baseGens
    updateEnvRaw (SMGetObject n) = testEnvStep . testEnvSerial (\s ->
        testEnvSetJ n testObjectType s)
    updateEnvRaw _ = testEnvStep
    actionRemote SMTrivial n =
        showCall n "trivial" id
    actionRemote (SMGetInt v) n =
        showTest n "getInt" (shows v)
    actionRemote (SMSetInt v) n =
        showCall n "setInt" (shows v)
    actionRemote (SMGetDouble v) n =
        showTest n "getDouble" (showJDouble v)
    actionRemote (SMSetDouble v) n =
        showCall n "setDouble" (showJDouble v)
    actionRemote (SMGetString v) n =
        showTest n "getString" (showJString v)
    actionRemote (SMSetString v) n =
        showCall n "setString" (showJString v)
    actionRemote (SMGetText v) n =
        showTest n "getText" (showJString $ T.unpack v)
    actionRemote (SMSetText v) n =
        showCall n "setText" (showJString $ T.unpack v)
    actionRemote (SMGetURI v) n =
        showTest n "getURI" (showJURI v)
    actionRemote (SMSetURI v) n =
        showCall n "setURI" (showJURI v)
    actionRemote (SMGetObject v) n =
        noScope $ showVarDecl v . showVarDot n . showString "getObject();\n"
    actionRemote (SMSetObject v) n =
        noScope $ showVarDot n . showString "setObject(" .
            showVar v . showString ");\n"
    mockObjDef = [
        defMethod "trivial" $ \m -> expectAction m $ \a -> case a of
            SMTrivial -> return $ Right ()
            _         -> return $ Left TBadActionCtor,
        defMethod "getInt" $ \m -> expectAction m $ \a -> case a of
            SMGetInt v -> return $ Right v
            _          -> return $ Left TBadActionCtor,
        defMethod "setInt" $ \m v -> expectAction m $ \a -> case a of
            SMSetInt w -> if v == w
                          then return $ Right ()
                          else return $ Left TBadActionData
            _          -> return $ Left TBadActionCtor,
        defMethod "getDouble" $ \m -> expectAction m $ \a -> case a of
            SMGetDouble v -> return $ Right v
            _             -> return $ Left TBadActionCtor,
        defMethod "setDouble" $ \m v -> expectAction m $ \a -> case a of
            SMSetDouble w -> if v == w
                             then return $ Right ()
                             else return $ Left TBadActionData
            _             -> return $ Left TBadActionCtor,
        defMethod "getString" $ \m -> expectAction m $ \a -> case a of
            SMGetString v -> return $ Right v
            _             -> return $ Left TBadActionCtor,
        defMethod "setString" $ \m v -> expectAction m $ \a -> case a of
            SMSetString w -> if v == w
                             then return $ Right ()
                             else return $ Left TBadActionData
            _             -> return $ Left TBadActionCtor,
        defMethod "getText" $ \m -> expectAction m $ \a -> case a of
            SMGetText v -> return $ Right v
            _           -> return $ Left TBadActionCtor,
        defMethod "setText" $ \m v -> expectAction m $ \a -> case a of
            SMSetText w -> if v == w
                           then return $ Right ()
                           else return $ Left TBadActionData
            _             -> return $ Left TBadActionCtor,
        defMethod "getURI" $ \m -> expectAction m $ \a -> case a of
            SMGetURI v -> return $ Right v
            _          -> return $ Left TBadActionCtor,
        defMethod "setURI" $ \m v -> expectAction m $ \a -> case a of
            SMSetURI w -> if v == w
                          then return $ Right ()
                          else return $ Left TBadActionData
            _             -> return $ Left TBadActionCtor,
        defMethod "getObject" $ \m -> expectAction m $ \a -> case a of
            SMGetObject v -> do
                status <- mockGetStatus m
                obj <- newObject (MockObj (testSerial status) $
                    mockStatus m :: MockObj TestObject)
                return $ Right obj
            _             -> return $ Left TBadActionCtor,
        defMethod "setObject" $ \m v -> expectAction m $ \a -> case a of
            SMSetObject w -> do
                status <- mockGetStatus m
                case IntMap.lookup w $ envJs $ testEnv status of
                    Just entry -> if entry == (
                        testObjectType, mockSerial (v :: MockObj TestObject))
                                  then return $ Right ()
                                  else return $ Left TBadActionData
                    _          -> return $ Left TBadActionSlot
            _             -> return $ Left TBadActionCtor]

data TestObject deriving Typeable

testObjectType :: TestType
testObjectType = TestType (Proxy :: Proxy TestObject)

instance Show TestObject where
   showsPrec _ = error "TestObject has no actions."

instance TestAction TestObject where
    legalActionIn _ _ = error "TestObject has no actions."
    nextActionsFor _ = Nothing
    updateEnvRaw _ e = e
    actionRemote _ _ = error "TestObject has no actions."
    mockObjDef = []
