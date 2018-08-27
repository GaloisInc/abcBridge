{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils ( rawSystemExit, rawSystemExitWithEnv
                                 , getDirectoryContentsRecursive
                                 , ordNub, isInfixOf, intercalate )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..), InstallDirs(..) )
import Distribution.PackageDescription (PackageDescription(..), GenericPackageDescription(..),
        HookedBuildInfo(..), BuildInfo(..), emptyBuildInfo,
#if MIN_VERSION_Cabal(2,2,0)
        lookupFlagAssignment,
#endif
        updatePackageDescription, FlagAssignment(..))
import Distribution.Verbosity (verbose, Verbosity(..))
import Distribution.System (OS(..), Arch(..), Platform (..), buildOS, buildPlatform)
import qualified Distribution.Simple.Utils
import System.Directory
import System.FilePath
import System.Environment( getEnvironment )
import Control.Monad ( filterM )
import Data.Maybe

#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Version( Version, showVersion )
import Distribution.PackageDescription (mkFlagName)
#else
import Data.Version( Version, showVersion )
import Distribution.PackageDescription (FlagName(..))
#endif

-- The abcBridge depends on the ABC library itself.  The ABC library can be
-- provided in two ways:
--
--    1. In the local abc-build subdirectory (usually populated via
--       git submodules)
--
--    2. It can already be present (e.g. as installed by system
--       package management).
--
-- This non-standard Setup will attempt to identify which of these
-- locations the ABC library can be obtained from, and prepares the
-- abcBridge build to utilize the include files and libabc.a file from
-- that location.  If the location is a local subdirectory, the cabal
-- configure will also build the libabc.a in that subdirectory (via
-- calling into the "scripts/build-abc.sh" script at the proper time).
--
-- This Setup will also dynamically modify the cabal package
-- description to include the ABC source files in the
-- `extra-source-files` specification (so `cabal sdist` works as
-- expected), and to add the ABC source tree directories to
-- `include-dirs`.  This is done by reading the files
-- `scripts/abc-sources.txt` and `scripts/abc-incl-dirs.txt`, which
-- are set up by `setupAbc` during `cabal configure`.
--
-- Finally, this Setup will also provide some information about where
-- to find the libabc.a and libabc.dll files.
--
-- The setup achieves all of this by the following:
--
--   * Using a configure hook to modify the package description read
--     from disk before returning the local build info that is used by
--     other cabal actions.
--
--   * Note that the sDistHook (postCopy) is not modified: the sdist
--     should not contain any libabc sources; those can be distributed
--     separately.
--
--   * The 'clean' action will also perform a clean of the local copy
--     of libabc.

main = defaultMainWithHooks simpleUserHooks
    {  confHook = \(gpkg_desc, hbi) f -> do
                    let v = fromFlag $ configVerbosity f
                    let fs = configConfigurationsFlags f
                    setupAbc v (packageDescription gpkg_desc)
                    -- Note: it is unusual to perform the build of the
                    -- dependency during the configure phase instead
                    -- of the build phase, but setup requires that all
                    -- dependencies are present and accessible.
                    buildAbc v fs
                    lbi <- confHook simpleUserHooks (gpkg_desc, hbi) f
                    pkg_desc' <- abcPkgDesc (localPkgDescr lbi)
                    return lbi{ localPkgDescr = pkg_desc' }

    -- See note above re building abc during the setup phase.
    -- , buildHook = \pkg_desc bi uh bf -> do
    --                 let v = fromFlag $ buildVerbosity bf
    --                     f = flagAssignment bi
    --                 buildAbc v f
    --                 buildHook simpleUserHooks pkg_desc bi uh bf

    , cleanHook = \pkg_desc unit uh cf -> do
                    let v = fromFlag $ cleanVerbosity cf
                    cleanAbc v
                    cleanHook simpleUserHooks pkg_desc unit uh cf

    , sDistHook = \pkg_desc lbi h f -> do
                    let v = fromFlag $ sDistVerbosity f
                    setupAbc v pkg_desc
                    pkg_desc' <- abcPkgDesc pkg_desc
                    sDistHook simpleUserHooks pkg_desc' lbi h f
    }

-- Determine which libabc should be used:
--  1. If there are sources available in the abc-build subdirectory,
--     use (and build) those.
--  2. If there is a LIBABC environment variable setting, expect that
--     to point to a (previously built) source tree
--  3. Look for the header files and library via the standard
--     compilation settings locations (including a Nix shell).

data ABCLib = LocalABC FilePath FilePath
            | SystemABC FilePath FilePath

getABCLib :: IO ABCLib
getABCLib = do
  let lclsrc = "abc-build" </> "src"
      libname = "libabc.a"
      hasABCincl p = doesFileExist $ p </> "base" </> "abc" </> "abc.h"
      chkABClib p = let f = (if take 2 p == "-L" then drop 2 p else p) </> libname
                    in doesFileExist f >>= \e -> return (if e then Just f else Nothing)
      noABCError w = error ("ABC library must be checked out as a submodule" ++
                            " or installed in the system (" ++ w ++ ").")
      envVals env = intercalate " " . concatMap (\v -> maybeToList $ lookup v env)
  lclsrcExists <- doesDirectoryExist lclsrc
  if lclsrcExists
    then do here <- getCurrentDirectory
            return $ LocalABC lclsrc $ here </> "abc-build"
    else do env <- getEnvironment
            let ilocs = concat [ map inclSub libabcenv
                                , words $ envVals env ["CFLAGS", "NIX_CFLAGS_COMPILE"]
                                , [ "/usr/include/abc" ]
                                ]
                llocs = concat [ map libSub libabcenv
                               , words $ envVals env ["LDFLAGS", "NIX_LDFLAGS"]
                               , [ "/usr/lib" ]
                               ]
                libabcenv = maybeToList $ lookup "LIBABC" env
                inclSub p = p </> "src"
                libSub = id
            abcInclDir <- ordNub <$> filterM hasABCincl ilocs
            abcLibDir <- ordNub . catMaybes <$> mapM chkABClib llocs
            case (abcInclDir, abcLibDir) of
              (i:[],l:[]) -> return $ SystemABC i $ l </> libname
              ([],_) -> noABCError "incl"
              (_,[]) -> noABCError "lib"
              _ -> error $ "Multiple ABC include locations found: " ++ show abcInclDir


-- Edit the package description to include the ABC source files,
-- ABC include directories, and static library directories.
abcPkgDesc :: PackageDescription -> IO PackageDescription
abcPkgDesc pkg_desc = do
  -- Note: assumes the script files have previously been built by setupAbc
  abcSrcFiles <- fmap lines $ readFile $ "scripts" </> "abc-sources.txt"
  abcInclDirs <- fmap lines $ readFile $ "scripts" </> "abc-incl-dirs.txt"
  (p,mkBI) <- getABCLib >>= \case
          LocalABC _ lib -> return (pkg_desc, libDirAbc lib)
          SystemABC _ lib -> let fullsrc = extraSrcFiles pkg_desc ++ abcSrcFiles
                                   in return (pkg_desc { extraSrcFiles = fullsrc }, libDirAbc lib)
  return $ updatePackageDescription (mkBI abcInclDirs) p

libDirAbc :: FilePath -> [FilePath] -> HookedBuildInfo
libDirAbc libdir abcInclDirs = (Just buildinfo, [])
    where buildinfo = emptyBuildInfo
                      { includeDirs = abcInclDirs
                      , extraLibDirs = [libdir]
                      }

onWindows :: Monad m => m () -> m ()
onWindows act = case buildPlatform of
                  Platform _ Windows -> act
                  _                  -> return ()

-- call "make clean" in the abc directory, if it exists
cleanAbc :: Verbosity -> IO ()
cleanAbc verbosity = do
    rawSystemExit verbosity "sh" ["scripts" </> "lite-clean-abc.sh"]

-- If necessary, fetch the ABC sources and prepare for building
setupAbc :: Verbosity -> PackageDescription -> IO ()
setupAbc verbosity pkg_desc = do
    putStrLn $ unwords ["Cabal library version:", showVersion Distribution.Simple.Utils.cabalVersion]
    let version = pkgVersion $ package $ pkg_desc
    let packageVersion = "PACKAGE_VERSION"

    abcSrcRoot <- getABCLib >>= \case
      LocalABC incl _ -> do putStrLn "Using libabc [to be built] in abc-build"
                            return incl
      SystemABC incl lib -> do putStrLn $ "Using libabc in " ++ show lib
                               return incl

    allSrcFiles <- let fullpath i = abcSrcRoot </> i
                   in map fullpath <$> getDirectoryContentsRecursive abcSrcRoot

    let isIncl = (==) ".h" . takeExtension
        inclDirs = ordNub . map takeDirectory . filter isIncl

    let isVCSDir d = any (\v -> isInfixOf v d) [ ".hg", ".git" ]
        isBinary f = takeExtension f `elem` [".hgignore", ".o", ".a", ".dll", ".lib"]
        sources = filter (not . isBinary) . filter (not . isVCSDir)

    writeFile ("scripts" </> "abc-incl-dirs.txt") $ unlines $ inclDirs allSrcFiles
    writeFile ("scripts" </> "abc-sources.txt") $ unlines $ sources allSrcFiles


-- Build the ABC library and put the files in the expected places
buildAbc :: Verbosity -> FlagAssignment -> IO ()
buildAbc verbosity fs = getABCLib >>= \case
  LocalABC _ _ -> do
#if MIN_VERSION_Cabal(2,2,0)
    let pthreads = maybe "0" (\x -> if x then "1" else "0") $ lookupFlagAssignment (mkFlagName "enable-pthreads") fs
#else
    let pthreads = maybe "0" (\x -> if x then "1" else "0") $ lookup (mkFlagName "enable-pthreads") fs
#endif
    env <- getEnvironment
    rawSystemExitWithEnv verbosity "sh"
        (("scripts"</>"build-abc.sh") : (tail . words . show $ buildPlatform))
        ([("PTHREADS",pthreads)] ++ filter ((/="PTHREADS") . fst) env)
  _ -> return ()  -- nothing to do when supplied by the system.


#if !(MIN_VERSION_Cabal(2,0,0))
mkFlagName :: String -> FlagName
mkFlagName = FlagName
#endif
