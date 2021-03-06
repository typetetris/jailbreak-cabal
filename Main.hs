{-# LANGUAGE CPP #-}
module Main ( main ) where

import System.Environment
import Distribution.Verbosity
import Distribution.PackageDescription.PrettyPrint
#if MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription.Parsec
#else
import Distribution.PackageDescription.Parse
#endif
import Distribution.Version

import Jailbreak

main :: IO ()
main = getArgs >>= mapM_ (\cabalFile -> readGenericPackageDescription silent cabalFile >>= writeGenericPackageDescription cabalFile . stripVersionRestrictions)

#if !MIN_VERSION_Cabal(2,0,0)
readGenericPackageDescription :: Verbosity -> String -> IO GenericPackageDescription
readGenericPackageDescription = readPackageDescription
#endif
