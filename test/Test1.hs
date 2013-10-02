{-# LANGUAGE DeriveDataTypeable, TypeFamilies #-}

module Main where

import Graphics.QML.Test.Framework
import Graphics.QML.Test.Harness
import Graphics.QML.Test.SimpleTest
import Data.Proxy
import System.Exit

main :: IO ()
main = do
    rs <- sequence [
        checkProperty $ TestType (Proxy :: Proxy SimpleMethods),
        checkProperty $ TestType (Proxy :: Proxy SimpleProperties)]
    if and rs
    then exitSuccess
    else exitFailure
