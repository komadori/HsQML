#!/usr/bin/runhaskell 
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Distribution.Simple
import Distribution.Simple.Compiler
import Distribution.Simple.BuildPaths
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Program.Ld
import Distribution.Simple.Register
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Text
import Distribution.Verbosity
import qualified Distribution.InstalledPackageInfo as I
import qualified Distribution.ModuleName as ModuleName
import Distribution.PackageDescription
import Language.Haskell.TH
import System.Environment
import qualified System.Info as Info
import System.FilePath

-- Use Template Haskell to support different versions of the Cabal API
$(let
    post4700BaseAPI = Info.compilerVersion >= Version [7,7] []
    post118CabalAPI = cabalVersion >= Version [1,17,0] []
    post122CabalAPI = cabalVersion >= Version [1,21,0] []
    post124CabalAPI = cabalVersion >= Version [1,23,0] []
    vnameE = VarE . mkName
    vnameP = VarP . mkName
    cnameE = ConE . mkName
    cnameP n = ConP (mkName n)
    app2E f x y = AppE (AppE f x) y
    app3E f x y z = AppE (app2E f x y) z
    app4E f x y z u = AppE (app3E f x y z) u
    app5E f x y z u v = AppE (app4E f x y z u) v
    nothingE = cnameE "Nothing"
    falseE = cnameE "False"
    -- 'setEnv' function was added in base 4.7.0.0
    setEnvShim = if post4700BaseAPI
        then vnameE "setEnv"
        else LamE [WildP, WildP] $ AppE (vnameE "return") (cnameE "()")
    -- 'LocalBuildInfo' record changed fields in Cabal 1.18
    extractCLBI = if post118CabalAPI
        then app2E (vnameE "getComponentLocalBuildInfo")
            (vnameE "x") (cnameE "CLibName")
        else AppE (vnameE "fromJust") $ AppE (vnameE "libraryConfig")
            (vnameE "x")
    -- 'ComponentLocalBuildInfo' record changed fields in Cabal 1.18 and 1.24
    getCompLibName =
        case () of
          _ | post124CabalAPI ->
                AppE (vnameE "getHSLibraryName") $
                AppE (vnameE "componentUnitId") (vnameE "clbi")
            | post118CabalAPI ->
                AppE (LamE [cnameP "LibraryName" [vnameP "n"]] (vnameE "n")) $
                AppE (vnameE "head") $
                AppE (vnameE "componentLibraries") (vnameE "clbi")
            | otherwise -> vnameE "def"
    -- 'programFindLocation' field changed signature in Cabal 1.18 and 1.24
    adaptFindLoc =
        case () of
          _ | post124CabalAPI ->
                LamE [vnameP "f", vnameP "x", WildP] $
                AppE (AppE (vnameE "fmap") (AppE (vnameE "fmap") (AppE
                    (AppE (vnameE "flip") (cnameE "(,)")) (cnameE "[]")))) $
                AppE (vnameE "f") (vnameE "x")
            | post118CabalAPI ->
                LamE [vnameP "f", vnameP "x", WildP] $
                AppE (vnameE "f") (vnameE "x")
            | otherwise -> vnameE "id"
    -- 'rawSystemStdInOut' function changed signature in Cabal 1.18
    rawSystemStdErr = if post118CabalAPI
        then app4E (app3E (vnameE "rawSystemStdInOut")
            (vnameE "v") (vnameE "p") (vnameE "a"))
            nothingE nothingE nothingE falseE 
        else app5E (vnameE "rawSystemStdInOut")
            (vnameE "v") (vnameE "p") (vnameE "a") nothingE falseE
    -- 'programPostConf' field changed signature in Cabal 1.18
    noPostConf = if post118CabalAPI
        then LamE [WildP, vnameP "c"] $ AppE (vnameE "return") (vnameE "c")
        else LamE [WildP, WildP] $ AppE (vnameE "return") (cnameE "[]")
    -- 'generateRegistrationInfo' function changed signature in Cabal 1.22
    genRegInfo = if post122CabalAPI
        then LamE [vnameP "pdb"] $ app2E (vnameE ">>=")
                (AppE (vnameE "absolutePackageDBPaths") (vnameE "pdb")) $
            LamE [vnameP "apdb"] $
            app5E (app4E (vnameE "generateRegistrationInfo")
                (vnameE "verb") (vnameE "pkg") (vnameE "lib") (vnameE "lbi"))
                (vnameE "clbi") (vnameE "inp")
                (AppE (vnameE "relocatable") (vnameE "lbi")) (vnameE "dir")
                (AppE (vnameE "registrationPackageDB") (vnameE "apdb"))
        else LamE [WildP] $ app3E (app4E (vnameE "generateRegistrationInfo")
            (vnameE "verb") (vnameE "pkg") (vnameE "lib") (vnameE "lbi"))
            (vnameE "clbi") (vnameE "inp") (vnameE "dir")
    -- 'registerPackage' function changed signature in Cabal 1.24
    regPkg = if post124CabalAPI
        then LamE [vnameP "verb", vnameP "ipi", vnameP "pkgDesc", vnameP "lbi",
                vnameP "inplace", vnameP "pkgDb"] $
            app3E (app3E (vnameE "registerPackage") (vnameE "verb")
                (AppE (vnameE "compiler") (vnameE "lbi"))
                (AppE (vnameE "withPrograms") (vnameE "lbi")))
                (vnameE "inplace") (vnameE "pkgDb") (vnameE "ipi")
        else vnameE "registerPackage"
    in return [
        FunD (mkName "setEnvShim") [
            Clause [] (NormalB setEnvShim) []],
        FunD (mkName "extractCLBI") [
            Clause [vnameP "x"] (NormalB extractCLBI) []],
        FunD (mkName "getCompLibName") [
            Clause [vnameP "clbi", vnameP "def"] (NormalB getCompLibName) []],
        FunD (mkName "adaptFindLoc") [
            Clause [] (NormalB adaptFindLoc) []],
        FunD (mkName "rawSystemStdErr") [
            Clause [vnameP "v", vnameP "p", vnameP "a"] (
                NormalB rawSystemStdErr) []],
        FunD (mkName "noPostConf") [
            Clause [] (NormalB noPostConf) []],
        FunD (mkName "genRegInfo") [
            Clause [vnameP "verb", vnameP "pkg", vnameP "lib", vnameP "lbi",
                vnameP "clbi", vnameP "inp", vnameP "dir"] (
                    NormalB genRegInfo) []],
        FunD (mkName "regPkg") [
            Clause [] (NormalB regPkg) []]])

main :: IO ()
main = do
  -- If system uses qtchooser(1) then encourage it to choose Qt 5
  env <- getEnvironment
  case lookup "QT_SELECT" env of
    Nothing -> setEnvShim "QT_SELECT" "5"
    _       -> return ()
  -- Chain standard setup
  defaultMainWithHooks simpleUserHooks {
    confHook = confWithQt, buildHook = buildWithQt,
    copyHook = copyWithQt, instHook = instWithQt,
    regHook = regWithQt}

getCustomStr :: String -> PackageDescription -> String
getCustomStr name pkgDesc =
  fromMaybe "" $ do
    lib <- library pkgDesc
    lookup name $ customFieldsBI $ libBuildInfo lib

getCustomFlag :: String -> PackageDescription -> Bool
getCustomFlag name pkgDesc =
  fromMaybe False . simpleParse $ getCustomStr name pkgDesc

xForceGHCiLib, xMocHeaders, xFrameworkDirs, xSeparateCbits :: String
xForceGHCiLib  = "x-force-ghci-lib"
xMocHeaders    = "x-moc-headers"
xFrameworkDirs = "x-framework-dirs"
xSeparateCbits = "x-separate-cbits"

confWithQt :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags ->
  IO LocalBuildInfo
confWithQt (gpd,hbi) flags = do
  let verb = fromFlag $ configVerbosity flags
  mocPath <- findMoc verb
  cppPath <- findProgramLocation verb "cpp"
  let mapLibBI = fmap . mapCondTree . mapBI $ substPaths mocPath cppPath
      gpd' = gpd {
        condLibrary = mapLibBI $ condLibrary gpd,
        condExecutables = mapAllBI mocPath cppPath $ condExecutables gpd,
        condTestSuites = mapAllBI mocPath cppPath $ condTestSuites gpd,
        condBenchmarks = mapAllBI mocPath cppPath $ condBenchmarks gpd}
  lbi <- confHook simpleUserHooks (gpd',hbi) flags
  -- Find Qt moc program and store in database
  (_,_,db') <- requireProgramVersion verb
    mocProgram qtVersionRange (withPrograms lbi)
  -- Force enable GHCi workaround library if flag set and not using shared libs
  let forceGHCiLib =
        (getCustomFlag xForceGHCiLib $ localPkgDescr lbi) &&
        (not $ withSharedLib lbi)
  -- Update LocalBuildInfo
  return lbi {withPrograms = db',
              withGHCiLib = withGHCiLib lbi || forceGHCiLib}

mapAllBI :: (HasBuildInfo a) => Maybe FilePath -> Maybe FilePath ->
  [(x, CondTree c v a)] -> [(x, CondTree c v a)]
mapAllBI mocPath cppPath =
  mapSnd . mapCondTree . mapBI $ substPaths mocPath cppPath

mapCondTree :: (a -> a) -> CondTree v c a -> CondTree v c a
mapCondTree f (CondNode val cnstr cs) =
  CondNode (f val) cnstr $ map updateChildren cs
  where updateChildren (cond,left,right) =
          (cond, mapCondTree f left, fmap (mapCondTree f) right)

substPaths :: Maybe FilePath -> Maybe FilePath -> BuildInfo -> BuildInfo
substPaths mocPath cppPath build =
  let escapeStr = init . tail . show
      toRoot = escapeStr . takeDirectory . takeDirectory . fromMaybe ""
  in read .
     replace "/QT_ROOT" (toRoot mocPath) .
     replace "/SYS_ROOT" (toRoot cppPath) .
     replace "-hide-option-" "-" $ show build

buildWithQt ::
  PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
buildWithQt pkgDesc lbi hooks flags = do
    let verb = fromFlag $ buildVerbosity flags
    libs' <- maybeMapM (\lib -> fmap (\lib' ->
      lib {libBuildInfo = lib'}) $ fixQtBuild verb lbi $ libBuildInfo lib) $
      library pkgDesc
    let pkgDesc' = pkgDesc {library = libs'}
        lbi' = if (needsGHCiFix pkgDesc lbi)
                 then lbi {withGHCiLib = False, splitObjs = False} else lbi
    buildHook simpleUserHooks pkgDesc' lbi' hooks flags
    case libs' of
      Just lib -> when (needsGHCiFix pkgDesc lbi) $
        buildGHCiFix verb pkgDesc lbi lib
      Nothing  -> return ()

fixQtBuild :: Verbosity -> LocalBuildInfo -> BuildInfo -> IO BuildInfo
fixQtBuild verb lbi build = do
  let moc  = fromJust $ lookupProgram mocProgram $ withPrograms lbi
      option name = words $ fromMaybe "" $ lookup name $ customFieldsBI build
      incs = option xMocHeaders
      bDir = buildDir lbi
      cpps = map (\inc ->
        bDir </> (takeDirectory inc) </>
        ("moc_" ++ (takeBaseName inc) ++ ".cpp")) incs
      args = map ("-I"++) (includeDirs build) ++
             map ("-F"++) (option xFrameworkDirs)
  -- Run moc on each of the header files containing QObject subclasses
  mapM_ (\(i,o) -> do
      createDirectoryIfMissingVerbose verb True (takeDirectory o)
      runProgram verb moc $ [i,"-o",o] ++ args) $ zip incs cpps
  -- Add the moc generated source files to be compiled
  return build {cSources = cpps ++ cSources build,
                ccOptions = "-fPIC" : ccOptions build}

needsGHCiFix :: PackageDescription -> LocalBuildInfo -> Bool
needsGHCiFix pkgDesc lbi =
  withGHCiLib lbi && getCustomFlag xSeparateCbits pkgDesc

mkGHCiFixLibPkgId :: PackageDescription -> PackageIdentifier
mkGHCiFixLibPkgId pkgDesc =
  let pid = packageId pkgDesc
      (PackageName name) = pkgName pid
  in pid {pkgName = PackageName $ "cbits-" ++ name}

mkGHCiFixLibName :: PackageDescription -> String
mkGHCiFixLibName pkgDesc =
  ("lib" ++ display (mkGHCiFixLibPkgId pkgDesc)) <.> dllExtension

mkGHCiFixLibRefName :: PackageDescription -> String
mkGHCiFixLibRefName pkgDesc =
  prefix ++ display (mkGHCiFixLibPkgId pkgDesc)
  where prefix = if dllExtension == "dll" then "lib" else ""

buildGHCiFix ::
  Verbosity -> PackageDescription -> LocalBuildInfo -> Library -> IO ()
buildGHCiFix verb pkgDesc lbi lib = do
  let bDir   = buildDir lbi
      ms     = map ModuleName.toFilePath $ libModules lib
      hsObjs = map ((bDir </>) . (<.> "o")) ms
      clbi   = extractCLBI lbi
      lname  = getCompLibName clbi $ ("HS" ++) $ display $ packageId pkgDesc
  stubObjs <- fmap catMaybes $
    mapM (findFileWithExtension ["o"] [bDir]) $ map (++ "_stub") ms
  (ld,_) <- requireProgram verb ldProgram (withPrograms lbi)
  combineObjectFiles verb ld (bDir </> lname <.> "o") (stubObjs ++ hsObjs)
  (ghc,_) <- requireProgram verb ghcProgram (withPrograms lbi)
  let bi = libBuildInfo lib
  runProgram verb ghc (
    ["-shared","-o",bDir </> (mkGHCiFixLibName pkgDesc)] ++
    (ldOptions bi) ++ (map ("-l" ++) $ extraLibs bi) ++
    (map ("-L" ++) $ extraLibDirs bi) ++
    (map ((bDir </>) . flip replaceExtension objExtension) $ cSources bi))
  return ()

mocProgram :: Program
mocProgram = Program {
  programName = "moc",
  programFindLocation = adaptFindLoc findMoc,
  programFindVersion = \verb path -> do
    (oLine,eLine,_) <- rawSystemStdErr verb path ["-v"]
    return $
      msum (map (\(p,l) -> findSubseq (stripPrefix p) l)
        [("(Qt ",eLine), ("moc-qt5 ",oLine), ("moc ",oLine)]) >>=
      simpleParse . takeWhile (\c -> isDigit c || c == '.'),
  programPostConf = noPostConf
}

findMoc :: Verbosity -> IO (Maybe FilePath)
findMoc verb =
    fmap msum $ mapM (findProgramLocation verb) ["moc-qt5", "moc"]

qtVersionRange :: VersionRange
qtVersionRange = intersectVersionRanges
  (orLaterVersion $ Version [5,0] []) (earlierVersion $ Version [6,0] [])

copyWithQt ::
  PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO ()
copyWithQt pkgDesc lbi hooks flags = do
  copyHook simpleUserHooks pkgDesc lbi hooks flags
  let verb = fromFlag $ copyVerbosity flags
      dest = fromFlag $ copyDest flags
      bDir = buildDir lbi
      libDir = libdir $ absoluteInstallDirs pkgDesc lbi dest
      file = mkGHCiFixLibName pkgDesc
  when (needsGHCiFix pkgDesc lbi) $
    installOrdinaryFile verb (bDir </> file) (libDir </> file)

regWithQt :: 
  PackageDescription -> LocalBuildInfo -> UserHooks -> RegisterFlags -> IO ()
regWithQt pkg@PackageDescription { library = Just lib } lbi _ flags = do
  let verb    = fromFlag $ regVerbosity flags
      inplace = fromFlag $ regInPlace flags
      dist    = fromFlag $ regDistPref flags
      pkgDb   = withPackageDB lbi
      clbi    = extractCLBI lbi
  instPkgInfo <- genRegInfo verb pkg lib lbi clbi inplace dist pkgDb
  let instPkgInfo' = instPkgInfo {
        -- Add extra library for GHCi workaround
        I.extraGHCiLibraries =
          (if needsGHCiFix pkg lbi then [mkGHCiFixLibRefName pkg] else []) ++
            I.extraGHCiLibraries instPkgInfo,
        -- Add directories to framework search path
        I.frameworkDirs =
          words (getCustomStr xFrameworkDirs pkg) ++
            I.frameworkDirs instPkgInfo}
  case flagToMaybe $ regGenPkgConf flags of
    Just regFile -> do
      writeUTF8File (fromMaybe (display (packageId pkg) <.> "conf") regFile) $
        I.showInstalledPackageInfo instPkgInfo'
    _ | fromFlag (regGenScript flags) ->
      die "Registration scripts are not implemented."
      | otherwise -> 
      regPkg verb instPkgInfo' pkg lbi inplace pkgDb
regWithQt pkgDesc _ _ flags =
  setupMessage (fromFlag $ regVerbosity flags) 
    "Package contains no library to register:" (packageId pkgDesc)

instWithQt ::
  PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags -> IO ()
instWithQt pkgDesc lbi hooks flags = do
  let copyFlags = defaultCopyFlags {
        copyDistPref   = installDistPref flags,
        copyVerbosity  = installVerbosity flags
      }
      regFlags = defaultRegisterFlags {
        regDistPref  = installDistPref flags,
        regInPlace   = installInPlace flags,
        regPackageDB = installPackageDB flags,
        regVerbosity = installVerbosity flags
      }
  copyWithQt pkgDesc lbi hooks copyFlags
  when (hasLibs pkgDesc) $ regWithQt pkgDesc lbi hooks regFlags

class HasBuildInfo a where
  mapBI :: (BuildInfo -> BuildInfo) -> a -> a

instance HasBuildInfo Library where
  mapBI f x = x {libBuildInfo = f $ libBuildInfo x} 

instance HasBuildInfo Executable where
  mapBI f x = x {buildInfo = f $ buildInfo x} 

instance HasBuildInfo TestSuite where
  mapBI f x = x {testBuildInfo = f $ testBuildInfo x} 

instance HasBuildInfo Benchmark where
  mapBI f x = x {benchmarkBuildInfo = f $ benchmarkBuildInfo x} 

maybeMapM :: (Monad m) => (a -> m b) -> (Maybe a) -> m (Maybe b)
maybeMapM f = maybe (return Nothing) $ liftM Just . f

mapSnd :: (a -> a) -> [(x, a)] -> [(x, a)]
mapSnd f = map (\(x,y) -> (x,f y))

findSubseq :: ([a] -> Maybe b) -> [a] -> Maybe b
findSubseq f [] = f []
findSubseq f xs@(_:ys) =
  case f xs of
    Nothing -> findSubseq f ys
    Just r  -> Just r

replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace [] _ xs = xs
replace _ _ [] = []
replace src dst xs@(x:xs') =
  case stripPrefix src xs of
    Just xs'' -> dst ++ replace src dst xs''
    Nothing  -> x : replace src dst xs'
