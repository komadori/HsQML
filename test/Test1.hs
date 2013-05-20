{-# LANGUAGE DeriveDataTypeable, TypeFamilies #-}

module Main where

import Graphics.QML.Test.Framework
import Graphics.QML.Test.Harness
import Graphics.QML.Test.SimpleTest
import Test.QuickCheck
import Data.Proxy

main :: IO ()
main = checkProperty $ TestType (Proxy :: Proxy SimpleMethods)
