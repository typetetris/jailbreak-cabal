{-# LANGUAGE CPP #-}

module Jailbreak ( stripVersionRestrictions ) where

import Distribution.Package
import Distribution.PackageDescription
#if MIN_VERSION_Cabal(1,25,0)
import Distribution.Types.CondTree
import Distribution.Types.Dependency
import Distribution.Types.ExeDependency
import Distribution.Types.LegacyExeDependency
#endif
import Distribution.Version


-- We don't relax version restrictions inside conditional statements.
-- See https://github.com/peti/jailbreak-cabal/commit/99eac40deb481b185fd93fd307625369ff5e1ec0
stripVersionRestrictions :: GenericPackageDescription -> GenericPackageDescription
stripVersionRestrictions pkg = pkg { condLibrary = fmap relaxLibraryTree (condLibrary pkg)
                                   , condSubLibraries = map (fmap relaxLibraryTree) (condSubLibraries pkg)
                                   , condExecutables = map (fmap relaxExeTree) (condExecutables pkg)
                                   , condTestSuites = map (fmap relaxTestTree) (condTestSuites pkg)
                                   }

relaxBranch :: (a -> a) -> CondBranch v [Dependency] a -> CondBranch v [Dependency] a
relaxBranch relaxTarget cb = cb { condBranchIfTrue  = relaxTreeConstraints relaxTarget (condBranchIfTrue cb) 
                                , condBranchIfFalse = fmap (relaxTreeConstraints relaxTarget) (condBranchIfFalse cb )
                                }

relaxTreeConstraints :: (a -> a) -> CondTree v [Dependency] a -> CondTree v [Dependency] a
relaxTreeConstraints relaxTarget ct =
                          ct { condTreeConstraints = map relax (condTreeConstraints ct)
                             , condTreeComponents = map (relaxBranch relaxTarget) (condTreeComponents ct)
                             , condTreeData = relaxTarget (condTreeData ct)
                             }

relaxLibraryTree :: CondTree v [Dependency] Library -> CondTree v [Dependency] Library
relaxLibraryTree ct = relaxTreeConstraints relaxLibrary ct

relaxExeTree :: CondTree v [Dependency] Executable -> CondTree v [Dependency] Executable
relaxExeTree ct = relaxTreeConstraints relaxExe ct

relaxTestTree :: CondTree v [Dependency] TestSuite -> CondTree v [Dependency] TestSuite
relaxTestTree ct = relaxTreeConstraints relaxTest ct

relaxLibrary :: Library -> Library
relaxLibrary l = l { libBuildInfo = relaxBuildInfo (libBuildInfo l) }

relaxExe :: Executable -> Executable
relaxExe e = e { buildInfo = relaxBuildInfo (buildInfo e) }

relaxTest :: TestSuite -> TestSuite
relaxTest t = t { testBuildInfo = relaxBuildInfo (testBuildInfo t) }

relaxBuildInfo :: BuildInfo -> BuildInfo
relaxBuildInfo bi = bi { buildTools = map relax (buildTools bi)
                       , buildToolDepends = map relax (buildToolDepends bi)
                       , targetBuildDepends = map relax (targetBuildDepends bi)
                       }

class DependencyType a where
  relax :: a -> a

instance DependencyType Dependency where
  relax (Dependency d _) = Dependency d anyVersion

#if MIN_VERSION_Cabal(1,25,0)

instance DependencyType LegacyExeDependency where
  relax (LegacyExeDependency d _) = LegacyExeDependency d anyVersion

instance DependencyType ExeDependency where
  relax (ExeDependency d ucn _) = ExeDependency d ucn anyVersion

#endif
