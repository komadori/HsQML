#!/usr/bin/runhaskell 
module Main where

import Data.List
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex
import Distribution.Simple.Setup
import Distribution.Simple.Utils (rawSystemExit)
import qualified Distribution.InstalledPackageInfo as I
import Distribution.PackageDescription
import System.Directory
import System.FilePath

import Control.Monad
import Data.Maybe
import qualified Distribution.ModuleName as ModuleName
import Distribution.Simple.BuildPaths
import Distribution.Simple.Program
import Distribution.Simple.Program.Ld
import Distribution.Simple.Register
import Distribution.Simple.Utils
import Distribution.Text
import Distribution.Version
import Distribution.Verbosity
import Text.Regex.Posix
import System.FilePath

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
  confHook = confWithQt, buildHook = buildWithQt,
  copyHook = copyWithQt, instHook = instWithQt,
  regHook = regWithQt}

confWithQt :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags ->
  IO LocalBuildInfo
confWithQt tuple flags = do
  lbi <- confHook simpleUserHooks tuple flags
  (_,_,db') <- requireProgramVersion (fromFlag $ configVerbosity flags)
      mocProgram qtVersionRange (withPrograms lbi)
  return $ lbi {withPrograms = db'}

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
        bDir </> ("moc_" ++ (takeBaseName inc) ++ ".cpp")) incs
  createDirectoryIfMissingVerbose verb True bDir
  mapM_ (\(i,o) -> runProgram verb moc [i,"-o",o]) $ zip incs cpps
  return build {cSources = cpps ++ cSources build}

needsGHCiFix :: PackageDescription -> LocalBuildInfo -> Bool
needsGHCiFix pkgDesc lbi =
  (withGHCiLib lbi &&) $ fromMaybe False $ do
    lib <- library pkgDesc
    str <- lookup "x-separate-cbits" $ customFieldsBI $ libBuildInfo lib
    simpleParse str

mkGHCiFixLibName :: PackageDescription -> String
mkGHCiFixLibName pkgDesc =
  ("libcbits-" ++ display (packageId pkgDesc)) <.> dllExtension

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
    (bDir </> (("HS" ++) $ display $ packageId pkgDesc) <.> ".o")
    (stubObjs ++ hsObjs)
  (ghc,_) <- requireProgram verb ghcProgram (withPrograms lbi)
  let bi = libBuildInfo lib
  runProgram verb ghc (
    ["-shared","-fPIC","-o",bDir </> (mkGHCiFixLibName pkgDesc)] ++
    (map ("-optc" ++) $ ccOptions bi) ++
    (map ("-optc" ++) $ cppOptions bi) ++
    (map ("-I" ++) $ includeDirs bi) ++
    (ldOptions bi) ++ (map ("-l" ++) $ extraLibs bi) ++
    (map ("-L"++) $ extraLibDirs bi) ++
    (cSources bi))
  return ()

mocProgram :: Program
mocProgram = Program {
  programName = "moc",
  programFindLocation = flip findProgramLocation "moc",
  programFindVersion = \verbosity path -> do
    (_,line,_) <- rawSystemStdInOut verbosity path ["-v"] Nothing False
    return $ case line =~ "\\(Qt ([0-9.]+)\\)" of
      (_:ver:_):_ -> simpleParse ver
      _           -> Nothing,
  programPostConf = \_ _ -> return []
}

qtVersionRange :: VersionRange
qtVersionRange = orLaterVersion $ Version [4,7] []

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
regWithQt pkg@PackageDescription { library       = Just lib  }
         lbi@LocalBuildInfo     { libraryConfig = Just clbi } hooks flags = do
  let verb    = fromFlag $ regVerbosity flags
      inplace = fromFlag $ regInPlace flags
      dist    = fromFlag $ regDistPref flags
      pkgDb   = registrationPackageDB $ withPackageDB lbi
  instPkgInfo <- generateRegistrationInfo
    verb pkg lib lbi clbi inplace dist
  let instPkgInfo' = if (needsGHCiFix pkg lbi)
        then instPkgInfo {I.extraGHCiLibraries =
          (drop 3 $ takeBaseName $ mkGHCiFixLibName pkg) :
          I.extraGHCiLibraries instPkgInfo}
        else instPkgInfo
  case flagToMaybe $ regGenPkgConf flags of
    Just regFile -> do
      let regFile = fromMaybe (display (packageId pkg) <.> "conf")
            (fromFlag $ regGenPkgConf flags)
      writeUTF8File regFile $ I.showInstalledPackageInfo instPkgInfo'
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
