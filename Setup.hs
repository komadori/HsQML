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

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
  buildHook = buildCbits, copyHook = copyCbits, preReg = regCbits }

buildCbits ::
  PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
buildCbits pkgDesc lbi hooks buildFlags = do
    pwd <- getCurrentDirectory
    let buildParam = "BUILD_DIR=" ++ (pwd </> buildDir lbi)
        rtses      = lookupPackageName (installedPkgs lbi) (PackageName "rts")
        includes   = I.includeDirs $ head $ snd $ head rtses
        hsffiParam = concat $ ("HSFFI_CFLAGS=" :) $
          intersperse " " $ map ("-I"++) includes
    rawSystemExit (fromFlag $ buildVerbosity buildFlags) "make"
        ["-C", "cbits", buildParam, hsffiParam]
    buildHook simpleUserHooks pkgDesc lbi hooks buildFlags

copyCbits ::
  PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO ()
copyCbits pkgDesc lbi hooks copyFlags = do
    let libPref = libdir $ absoluteInstallDirs pkgDesc lbi $
          fromFlag $ copyDest copyFlags
    rawSystemExit (fromFlag $ copyVerbosity copyFlags) "make"
        ["-C", buildDir lbi, "INSTALL_ROOT="++libPref, "install"]
    copyHook simpleUserHooks pkgDesc lbi hooks copyFlags

regCbits :: Args -> RegisterFlags -> IO HookedBuildInfo
regCbits _ _ =
    return (Just emptyBuildInfo {extraLibs = ["hsqml-cbits"]}, [])
