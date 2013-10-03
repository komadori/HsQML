{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}

module Graphics.QML.Test.SimpleTest where

import Graphics.QML.Objects
import Graphics.QML.Test.Framework
import Graphics.QML.Test.MayGen
import Graphics.QML.Test.GenURI

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Control.Applicative
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

showGet :: Int -> String -> ShowS -> (ShowS, ShowS)
showGet n name value = noScope $
    showString "if (" . showVarDot n . showString name .
    showString " != " . value . showString ") {window.close();}\n"

showSet :: Int -> String -> ShowS -> (ShowS, ShowS)
showSet n name value = noScope $
    showVarDot n . showString name .
    showString " = " . value . showString ";\n"

checkArg :: (Show a, Eq a) => a -> a -> IO (Either TestFault ())
checkArg v w = return $
    if v == w then Right () else Left TBadActionData

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
    legalActionIn (SMSetObject n) env = testEnvIsaJ n testObjectType env
    legalActionIn _ _ = True
    nextActionsFor env = mayOneof [
        pure SMTrivial,
        SMGetInt <$> fromGen arbitrary,
        SMSetInt <$> fromGen arbitrary,
        SMGetDouble <$> fromGen arbitrary,
        SMSetDouble <$> fromGen arbitrary,
        SMGetString <$> fromGen arbitrary,
        SMSetString <$> fromGen arbitrary,
        SMGetText . T.pack <$> fromGen arbitrary,
        SMSetText . T.pack <$> fromGen arbitrary,
        SMGetURI <$> fromGen uriGen,
        SMSetURI <$> fromGen uriGen,
        pure . SMGetObject $ testEnvNextJ env,
        SMSetObject <$> mayElements (testEnvListJ testObjectType env)]
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
        showCall n "setObject" (showVar v)
    mockObjDef = [
        defMethod "trivial" $ \m -> expectAction m $ \a -> case a of
            SMTrivial -> return $ Right ()
            _         -> return $ Left TBadActionCtor,
        defMethod "getInt" $ \m -> expectAction m $ \a -> case a of
            SMGetInt v -> return $ Right v
            _          -> return $ Left TBadActionCtor,
        defMethod "setInt" $ \m v -> expectAction m $ \a -> case a of
            SMSetInt w -> checkArg v w
            _          -> return $ Left TBadActionCtor,
        defMethod "getDouble" $ \m -> expectAction m $ \a -> case a of
            SMGetDouble v -> return $ Right v
            _             -> return $ Left TBadActionCtor,
        defMethod "setDouble" $ \m v -> expectAction m $ \a -> case a of
            SMSetDouble w -> checkArg v w
            _             -> return $ Left TBadActionCtor,
        defMethod "getString" $ \m -> expectAction m $ \a -> case a of
            SMGetString v -> return $ Right v
            _             -> return $ Left TBadActionCtor,
        defMethod "setString" $ \m v -> expectAction m $ \a -> case a of
            SMSetString w -> checkArg v w
            _             -> return $ Left TBadActionCtor,
        defMethod "getText" $ \m -> expectAction m $ \a -> case a of
            SMGetText v -> return $ Right v
            _           -> return $ Left TBadActionCtor,
        defMethod "setText" $ \m v -> expectAction m $ \a -> case a of
            SMSetText w -> checkArg v w
            _           -> return $ Left TBadActionCtor,
        defMethod "getURI" $ \m -> expectAction m $ \a -> case a of
            SMGetURI v -> return $ Right v
            _          -> return $ Left TBadActionCtor,
        defMethod "setURI" $ \m v -> expectAction m $ \a -> case a of
            SMSetURI w -> checkArg v w
            _          -> return $ Left TBadActionCtor,
        defMethod "getObject" $ \m -> expectAction m $ \a -> case a of
            SMGetObject _ -> getTestObject m
            _             -> return $ Left TBadActionCtor,
        defMethod "setObject" $ \m v -> expectAction m $ \a -> case a of
            SMSetObject w -> setTestObject m v w
            _             -> return $ Left TBadActionCtor]

data SimpleProperties
    = SPGetIntRO Int32
    | SPGetInt Int32
    | SPSetInt Int32
    | SPGetDouble Double
    | SPSetDouble Double
    | SPGetString String
    | SPSetString String
    | SPGetText Text
    | SPSetText Text
    | SPGetURI URI
    | SPSetURI URI
    | SPGetObject Int
    | SPSetObject Int
    deriving (Show, Typeable)

instance TestAction SimpleProperties where
    legalActionIn (SPSetObject n) env = testEnvIsaJ n testObjectType env
    legalActionIn _ _ = True
    nextActionsFor env = mayOneof [
        SPGetIntRO <$> fromGen arbitrary,
        SPGetInt <$> fromGen arbitrary,
        SPSetInt <$> fromGen arbitrary,
        SPGetDouble <$> fromGen arbitrary,
        SPSetDouble <$> fromGen arbitrary,
        SPGetString <$> fromGen arbitrary,
        SPSetString <$> fromGen arbitrary,
        SPGetText . T.pack <$> fromGen arbitrary,
        SPSetText . T.pack <$> fromGen arbitrary,
        SPGetURI <$> fromGen uriGen,
        SPSetURI <$> fromGen uriGen,
        pure . SPGetObject $ testEnvNextJ env,
        SPSetObject <$> mayElements (testEnvListJ testObjectType env)]
    updateEnvRaw (SPGetObject n) = testEnvStep . testEnvSerial (\s ->
        testEnvSetJ n testObjectType s)
    updateEnvRaw _ = testEnvStep
    actionRemote (SPGetIntRO v) n =
        showGet n "propIntRO" (shows v)
    actionRemote (SPGetInt v) n =
        showGet n "propIntR" (shows v)
    actionRemote (SPSetInt v) n =
        showSet n "propIntW" (shows v)
    actionRemote (SPGetDouble v) n =
        showGet n "propDoubleR" (showJDouble v)
    actionRemote (SPSetDouble v) n =
        showSet n "propDoubleW" (showJDouble v)
    actionRemote (SPGetString v) n =
        showGet n "propStringR" (showJString v)
    actionRemote (SPSetString v) n =
        showSet n "propStringW" (showJString v)
    actionRemote (SPGetText v) n =
        showGet n "propTextR" (showJString $ T.unpack v)
    actionRemote (SPSetText v) n =
        showSet n "propTextW" (showJString $ T.unpack v)
    actionRemote (SPGetURI v) n =
        showGet n "propURIR" (showJURI v)
    actionRemote (SPSetURI v) n =
        showSet n "propURIW" (showJURI v)
    actionRemote (SPGetObject v) n =
        noScope $ showVarDecl v . showVarDot n . showString "propObjectR;\n"
    actionRemote (SPSetObject v) n =
        showSet n "propObjectW" (showVar v)
    mockObjDef = [
        -- There are seperate properties for testing accessors and mutators
        -- becasue QML produces spurious reads when writing.
        defPropertyRO "propIntRO"
            (\m -> expectAction m $ \a -> case a of
                SPGetIntRO v -> return $ Right v
                _            -> return $ Left TBadActionCtor),
        defPropertyRW "propIntR"
            (\m -> expectAction m $ \a -> case a of
                SPGetInt v -> return $ Right v
                _          -> return $ Left TBadActionCtor)
            (\m _ -> badAction m),
        defPropertyRW "propIntW"
            (\m -> makeDef)
            (\m v -> expectAction m $ \a -> case a of
                SPSetInt w -> checkArg v w
                _          -> return $ Left TBadActionCtor),
        defPropertyRW "propDoubleR"
            (\m -> expectAction m $ \a -> case a of
                SPGetDouble v -> return $ Right v
                _             -> return $ Left TBadActionCtor)
            (\m _ -> badAction m),
        defPropertyRW "propDoubleW"
            (\m -> makeDef)
            (\m v -> expectAction m $ \a -> case a of
                SPSetDouble w -> checkArg v w
                _             -> return $ Left TBadActionCtor),
        defPropertyRW "propStringR"
            (\m -> expectAction m $ \a -> case a of
                SPGetString v -> return $ Right v
                _             -> return $ Left TBadActionCtor)
            (\m _ -> badAction m),
        defPropertyRW "propStringW"
            (\m -> makeDef)
            (\m v -> expectAction m $ \a -> case a of
                SPSetString w -> checkArg v w
                _             -> return $ Left TBadActionCtor),
        defPropertyRW "propTextR"
            (\m -> expectAction m $ \a -> case a of
                SPGetText v -> return $ Right v
                _           -> return $ Left TBadActionCtor)
            (\m _ -> badAction m),
        defPropertyRW "propTextW"
            (\m -> makeDef)
            (\m v -> expectAction m $ \a -> case a of
                SPSetText w -> checkArg v w
                _           -> return $ Left TBadActionCtor),
        defPropertyRW "propURIR"
            (\m -> expectAction m $ \a -> case a of
                SPGetURI v -> return $ Right v
                _          -> return $ Left TBadActionCtor)
            (\m _ -> badAction m),
        defPropertyRW "propURIW"
            (\m -> makeDef)
            (\m v -> expectAction m $ \a -> case a of
                SPSetURI w -> checkArg v w
                _          -> return $ Left TBadActionCtor),
        defPropertyRW "propObjectR"
            (\m -> expectAction m $ \a -> case a of
                SPGetObject _ -> getTestObject m
                _             -> return $ Left TBadActionCtor)
            (\m _ -> badAction m),
        defPropertyRW "propObjectW"
            (\m -> makeDef)
            (\m v -> expectAction m $ \a -> case a of
                SPSetObject w -> setTestObject m (fromObjRef v) w
                _             -> return $ Left TBadActionCtor)]

data TestObject deriving Typeable

testObjectType :: TestType
testObjectType = TestType (Proxy :: Proxy TestObject)

getTestObject ::
    MockObj a -> IO (Either TestFault (ObjRef (MockObj TestObject)))
getTestObject m = do
    status <- mockGetStatus m
    obj <- newObject (MockObj (testSerial status) $
        mockStatus m :: MockObj TestObject)
    return $ Right obj

setTestObject :: 
    MockObj a -> MockObj TestObject -> Int -> IO (Either TestFault ())
setTestObject m v w = do
    status <- mockGetStatus m
    case IntMap.lookup w $ envJs $ testEnv status of
        Just entry -> if entry == (testObjectType, mockSerial v)
                      then return $ Right ()
                      else return $ Left TBadActionData
        _          -> return $ Left TBadActionSlot

instance Show TestObject where
    showsPrec _ = error "TestObject has no actions."

instance TestAction TestObject where
    legalActionIn _ _ = error "TestObject has no actions."
    nextActionsFor _ = noGen
    updateEnvRaw _ e = e
    actionRemote _ _ = error "TestObject has no actions."
    mockObjDef = []
