{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.List ( nub )
--import Distribution.Types.Version( showVersion )
import Distribution.Pretty (prettyShow)
import Distribution.Types.PackageName (unPackageName)
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Types.UnitId (UnitId)
import Distribution.Types.MungedPackageId (MungedPackageId(mungedName, mungedVersion))
import Distribution.Types.MungedPackageName (encodeCompatPackageName)
import Distribution.PackageDescription ( PackageDescription(), TestSuite(..) )
import Distribution.Simple ( defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.Simple.Utils ( rewriteFileEx, createDirectoryIfMissingVerbose )
import Distribution.Simple.BuildPaths ( autogenPackageModulesDir )
import Distribution.Simple.Setup ( BuildFlags(buildVerbosity), fromFlag )
import Distribution.Simple.LocalBuildInfo ( withLibLBI, withTestLBI, LocalBuildInfo(), ComponentLocalBuildInfo(componentPackageDeps) )
import Distribution.Verbosity ( Verbosity )
import System.FilePath ( (</>) )

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pkg lbi hooks flags -> do
     generateBuildModule (fromFlag (buildVerbosity flags)) pkg lbi
     buildHook simpleUserHooks pkg lbi hooks flags
  }

generateBuildModule :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
generateBuildModule verbosity pkg lbi = do
  let dir = autogenPackageModulesDir lbi
  createDirectoryIfMissingVerbose verbosity True dir
  withLibLBI pkg lbi $ \_ libcfg -> do
    withTestLBI pkg lbi $ \suite suitecfg -> do
      let testNameSuite = unUnqualComponentName $ testName suite
      rewriteFileEx verbosity (dir </> "Build_" ++ testNameSuite ++ ".hs") $ unlines
        [ "module Build_" ++ testNameSuite ++ " where"
        , "deps :: [String]"
        , "deps = " ++ (show $ formatdeps (testDeps libcfg suitecfg))
        ]
  where
    formatdeps = map (formatone . snd)
    formatone p = let n = unPackageName  (encodeCompatPackageName $ mungedName p) 
                   in n ++ "-" ++ prettyShow (mungedVersion p)

testDeps :: ComponentLocalBuildInfo -> ComponentLocalBuildInfo -> [(UnitId, MungedPackageId)]
testDeps xs ys = nub $ componentPackageDeps xs ++ componentPackageDeps ys

