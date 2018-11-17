{-# LANGUAGE CPP #-}
module Main ( main ) where

import Distribution.Verbosity
import Distribution.PackageDescription.PrettyPrint
#if MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription.Parsec
#else
import Distribution.PackageDescription.Parse
#endif
import Distribution.Version
import System.Exit

import Jailbreak

main :: IO ()
main = do
  source   <- readGenericPackageDescription silent "testData/cabal-plan.cabal.normalized"
  expected <- readFile "testData/cabal-plan.cabal.stripped"
  let processed = showGenericPackageDescription . stripVersionRestrictions $ source
  if processed /= expected
  then exitFailure
  else exitSuccess

#if !MIN_VERSION_Cabal(2,0,0)
readGenericPackageDescription :: Verbosity -> String -> IO GenericPackageDescription
readGenericPackageDescription = readPackageDescription
#endif
