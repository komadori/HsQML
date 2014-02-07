#!/usr/bin/runhaskell 
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Distribution.Simple
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
import System.FilePath

-- Use Template Haskell to support both the pre- and post-1.18 Cabal API
$(let
    newCabalAPI = cabalVersion >= Version [1,17,0] []
    vnameE = VarE . mkName
    vnameP = VarP . mkName
    cnameE = ConE . mkName
    app2E f x y = AppE (AppE f x) y
    app3E f x y z = AppE (app2E f x y) z
    nothingE = cnameE "Nothing"
    falseE = cnameE "False"
    -- 'LocalBuildInfo' record changed fields in Cabal 1.18
    extractCLBI = if newCabalAPI
        then app2E (vnameE "getComponentLocalBuildInfo")
            (vnameE "x") (cnameE "CLibName")
        else AppE (vnameE "fromJust") $ AppE (vnameE "libraryConfig")
            (vnameE "x")
    -- 'programFindLocation' field changed signature in Cabal 1.18
    adaptFindLoc = if newCabalAPI
        then LamE [vnameP "f", vnameP "x", WildP] $
            AppE (vnameE "f") (vnameE "x")
        else vnameE "id"
    -- 'rawSystemStdInOut' function changed signature in Cabal 1.18
    rawSystemStdErr = if newCabalAPI
        then AppE (app3E (app3E (vnameE "rawSystemStdInOut")
            (vnameE "v") (vnameE "p") (vnameE "a"))
            nothingE nothingE nothingE) falseE 
        else app2E (app3E (vnameE "rawSystemStdInOut")
            (vnameE "v") (vnameE "p") (vnameE "a")) nothingE falseE
    -- 'programPostConf' field changed signature in Cabal 1.18
    noPostConf = if newCabalAPI
        then LamE [WildP, vnameP "c"] $ AppE (vnameE "return") (vnameE "c")
        else LamE [WildP, WildP] $ AppE (vnameE "return") (cnameE "[]")
    in return [
        FunD (mkName "extractCLBI") [
            Clause [vnameP "x"] (NormalB extractCLBI) []],
        FunD (mkName "adaptFindLoc") [
            Clause [] (NormalB adaptFindLoc) []],
        FunD (mkName "rawSystemStdErr") [
            Clause [vnameP "v", vnameP "p", vnameP "a"] (
                NormalB rawSystemStdErr) []],
        FunD (mkName "noPostConf") [
            Clause [] (NormalB noPostConf) []]])

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
  confHook = confWithQt, buildHook = buildWithQt,
  copyHook = copyWithQt, instHook = instWithQt,
  regHook = regWithQt}

getCustomFlag :: String -> PackageDescription -> Bool
getCustomFlag name pkgDesc =
  fromMaybe False $ do
    lib <- library pkgDesc
    str <- lookup name $ customFieldsBI $ libBuildInfo lib
    simpleParse str

confWithQt :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags ->
  IO LocalBuildInfo
confWithQt (gpd,hbi) flags = do
  let verb = fromFlag $ configVerbosity flags
  mocPath <- findProgramLocation verb "moc"
  cppPath <- findProgramLocation verb "cpp"
  let condLib = fromJust $ condLibrary gpd
      fixCondLib lib = lib {
        libBuildInfo = substPaths mocPath cppPath $ libBuildInfo lib} 
      condLib' = mapCondTree fixCondLib condLib
      gpd' = gpd {condLibrary = Just $ condLib'}
  lbi <- confHook simpleUserHooks (gpd',hbi) flags
  -- Find Qt moc program and store in database
  (_,_,db') <- requireProgramVersion verb
    mocProgram qtVersionRange (withPrograms lbi)
  -- Force enable GHCi workaround library if flag set and not using shared libs 
  let forceGHCiLib =
        (getCustomFlag "x-force-ghci-lib" $ localPkgDescr lbi) &&
        (not $ withSharedLib lbi)
  -- Update LocalBuildInfo
  return lbi {withPrograms = db',
              withGHCiLib = withGHCiLib lbi || forceGHCiLib}

mapCondTree :: (a -> a) -> CondTree v c a -> CondTree v c a
mapCondTree f (CondNode val cnstr cs) =
  CondNode (f val) cnstr $ map updateChildren cs
  where updateChildren (cond,left,right) =
          (cond, mapCondTree f left, fmap (mapCondTree f) right)

substPaths :: Maybe FilePath -> Maybe FilePath -> BuildInfo -> BuildInfo
substPaths mocPath cppPath build =
  let toRoot = takeDirectory . takeDirectory . fromMaybe ""
      substPath = replacePrefix "/QT_ROOT" (toRoot mocPath) .
        replacePrefix "/SYS_ROOT" (toRoot cppPath)
  in build {extraLibDirs = map substPath $ extraLibDirs build,
            includeDirs = map substPath $ includeDirs build}

replacePrefix :: (Eq a) => [a] -> [a] -> [a] -> [a]
replacePrefix old new xs =
  case stripPrefix old xs of
    Just ys -> new ++ ys
    Nothing -> xs

buildWithQt ::
  PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
buildWithQt pkgDesc lbi hooks flags = do
    let verb = fromFlag $ buildVerbosity flags
    libs' <- maybeMapM (\lib -> fmap (\lib' ->
      lib {libBuildInfo = lib'}) $ fixQtBuild verb lbi $ libBuildInfo lib) $
      library pkgDesc
    exes' <- mapM (\exe -> fmap (\exe' ->
      exe {buildInfo = exe'}) $ fixQtBuild verb lbi $ buildInfo exe) $
      executables pkgDesc
    let pkgDesc' = pkgDesc {library = libs', executables = exes'}
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
      incs = words $ fromMaybe "" $ lookup "x-moc-headers" $
        customFieldsBI build
      bDir = buildDir lbi
      cpps = map (\inc ->
        bDir </> (takeDirectory inc) </>
        ("moc_" ++ (takeBaseName inc) ++ ".cpp")) incs
  mapM_ (\(i,o) -> do
      createDirectoryIfMissingVerbose verb True (takeDirectory o)
      runProgram verb moc [i,"-o",o]) $ zip incs cpps
  return build {cSources = cpps ++ cSources build,
                ccOptions = "-fPIC" : ccOptions build}

needsGHCiFix :: PackageDescription -> LocalBuildInfo -> Bool
needsGHCiFix pkgDesc lbi =
  withGHCiLib lbi && getCustomFlag "x-separate-cbits" pkgDesc

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
  let bDir = buildDir lbi
      ms = map ModuleName.toFilePath $ libModules lib
      hsObjs = map ((bDir </>) . (<.> "o")) ms
  stubObjs <- fmap catMaybes $
    mapM (findFileWithExtension ["o"] [bDir]) $ map (++ "_stub") ms
  (ld,_) <- requireProgram verb ldProgram (withPrograms lbi)
  combineObjectFiles verb ld
    (bDir </> (("HS" ++) $ display $ packageId pkgDesc) <.> "o")
    (stubObjs ++ hsObjs)
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
  programFindLocation = adaptFindLoc $
    \verb -> findProgramLocation verb "moc",
  programFindVersion = \verb path -> do
    (oLine,eLine,_) <- rawSystemStdErr verb path ["-v"]
    return $
      (findSubseq (stripPrefix "(Qt ") eLine `mplus`
       findSubseq (stripPrefix "moc ") oLine) >>=
      simpleParse . takeWhile (\c -> isDigit c || c == '.'),
  programPostConf = noPostConf
}

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
  instPkgInfo <- generateRegistrationInfo
    verb pkg lib lbi clbi inplace dist
  let instPkgInfo' = if (needsGHCiFix pkg lbi)
        then instPkgInfo {I.extraGHCiLibraries =
          mkGHCiFixLibRefName pkg : I.extraGHCiLibraries instPkgInfo}
        else instPkgInfo
  case flagToMaybe $ regGenPkgConf flags of
    Just regFile -> do
      writeUTF8File (fromMaybe (display (packageId pkg) <.> "conf") regFile) $
        I.showInstalledPackageInfo instPkgInfo'
    _ | fromFlag (regGenScript flags) ->
      die "Registration scripts are not implemented."
      | otherwise -> 
      registerPackage verb instPkgInfo' pkg lbi inplace pkgDb
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

maybeMapM :: (Monad m) => (a -> m b) -> (Maybe a) -> m (Maybe b)
maybeMapM f = maybe (return Nothing) $ liftM Just . f

findSubseq :: ([a] -> Maybe b) -> [a] -> Maybe b
findSubseq f [] = f []
findSubseq f xs@(_:ys) =
  case f xs of
    Nothing -> findSubseq f ys
    Just r  -> Just r
