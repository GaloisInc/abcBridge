import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils (rawSystemExit, rawSystemExitWithEnv, installOrdinaryFile,
        installExecutableFile, copyFileVerbose, createDirectoryIfMissingVerbose)
import Distribution.Simple.LocalBuildInfo (
        LocalBuildInfo(..), InstallDirs(..), absoluteInstallDirs)
import Distribution.PackageDescription (PackageDescription(..), GenericPackageDescription(..),
        HookedBuildInfo(..), BuildInfo(..), emptyBuildInfo,
        updatePackageDescription, FlagAssignment(..), FlagName(..))
import Distribution.Verbosity (verbose, Verbosity(..))
import Distribution.System (OS(..), Arch(..), Platform (..), buildOS, buildPlatform)
import Data.Version( Version, showVersion )
import System.Directory
import System.FilePath
import System.Environment( getEnvironment )
import Control.Monad(when)

-- Here we install custom hooks to deal with fetching and building the ABC
-- sources.  This mostly involves calling into the "scripts/setup-abc.sh"
-- and "scripts/build-abc.sh" scripts at the proper times.
-- 
-- The other thing we must do is automagically munge the cabal description
-- to handle the ABC source tree.  We do this by editing, at runtime, the
-- cabal package description to include the ABC sources files to `extra-source-files`
-- (so `cabal sdist` works as expected), and to add the ABC source tree directories
-- to `include-dirs`.  This is done by reading the files `abc-build/abc-sources.txt`
-- and `abc-build/abc-incl-dirs.txt`, which are set up by `scripts/setup-abc.sh`.
--
-- Finally, we must also include some information about where do find the libabc.a
-- and libabc.dll files.
--
-- The way we have to do this is sort of nasty, but implementing the "readDesc" user
-- hook doesn't seem to work the way I expect, so we have to modify the package description
-- "in flight" inside each of the various hooks.

main = defaultMainWithHooks simpleUserHooks
    { postConf = \a f pkg_desc lbi -> do
                    pkg_desc' <- abcPkgDesc pkg_desc
                    postConf simpleUserHooks a f pkg_desc' lbi

    ,  confHook = \(gpkg_desc, hbi) f -> do
                    let v = fromFlag $ configVerbosity f
                    let fs = configConfigurationsFlags f
                    setupAbc v (packageDescription gpkg_desc)
                    buildAbc v fs
                    confHook simpleUserHooks (gpkg_desc, hbi) f

    , sDistHook = \pkg_desc lbi h f -> do
                    let v = fromFlag $ sDistVerbosity f
                    setupAbc v pkg_desc
                    pkg_desc' <- abcPkgDesc pkg_desc
                    sDistHook simpleUserHooks pkg_desc' lbi h f

    , buildHook = \pkg_desc lbi h f -> do
                    pkg_desc' <- abcPkgDesc pkg_desc
                    buildHook simpleUserHooks pkg_desc' lbi h f
    , haddockHook = \pkg_desc lbi h f -> do
                    pkg_desc' <- abcPkgDesc pkg_desc
                    haddockHook simpleUserHooks pkg_desc' lbi h f
    , copyHook = \pkg_desc lbi h f -> do
                    pkg_desc' <- abcPkgDesc pkg_desc
                    copyHook simpleUserHooks pkg_desc' lbi h f
    , instHook = \pkg_desc lbi h f -> do
                    pkg_desc' <- abcPkgDesc pkg_desc
                    instHook simpleUserHooks pkg_desc' lbi h f
    , regHook = \pkg_desc lbi h f -> do
                    pkg_desc' <- abcPkgDesc pkg_desc
                    regHook simpleUserHooks pkg_desc' lbi h f
    , unregHook = \pkg_desc lbi h f -> do
                    pkg_desc' <- abcPkgDesc pkg_desc
                    unregHook simpleUserHooks pkg_desc' lbi h f
    , hscolourHook = \pkg_desc lbi h f -> do
                    pkg_desc' <- abcPkgDesc pkg_desc
                    hscolourHook simpleUserHooks pkg_desc' lbi h f
    , testHook = \pkg_desc lbi h f -> do
                    pkg_desc' <- abcPkgDesc pkg_desc
                    testHook simpleUserHooks pkg_desc' lbi h f

    , postCopy = postCopyAbc
    , postInst = postInstAbc
    }

-- This is where we stash the static compiled ABC libraries
static_dir = "dist"</>"build"

-- Edit the package description to include the ABC source files,
-- ABC include directories, and static library directories.
abcPkgDesc :: PackageDescription -> IO PackageDescription
abcPkgDesc pkg_desc = do
  cwd <- getCurrentDirectory
  abcSrcFiles <- fmap lines $ readFile $ "abc-build" </> "abc-sources.txt"
  abcInclDirs <- fmap lines $ readFile $ "abc-build" </> "abc-incl-dirs.txt"
  let pg' = updatePackageDescription (libDirAbc cwd abcInclDirs) pkg_desc
  return pg'{ extraSrcFiles = extraSrcFiles pg' ++ abcSrcFiles 
            }

libDirAbc :: FilePath -> [FilePath] -> HookedBuildInfo
libDirAbc cwd abcInclDirs = (Just buildinfo, [])
    where buildinfo = emptyBuildInfo 
                      { includeDirs = abcInclDirs
                      , extraLibDirs = [cwd </> static_dir]
                      }

onWindows :: Monad m => m () -> m ()
onWindows act = case buildPlatform of
                  Platform _ Windows -> act
                  _                  -> return ()

-- If necessary, fetch the ABC sources and prepare for building
setupAbc :: Verbosity -> PackageDescription -> IO ()
setupAbc verbosity pkg_desc = do
    let version = pkgVersion $ package $ pkg_desc
    let packageVersion = "PACKAGE_VERSION"
    env <- getEnvironment
    rawSystemExitWithEnv verbosity "sh"
        ( ("scripts" </> "setup-abc.sh") : (tail . words . show $ buildPlatform))
        ([(packageVersion, showVersion version)] ++ filter ((/=packageVersion) . fst) env)

-- Build the ABC library and put the files in the expected places
buildAbc :: Verbosity -> FlagAssignment -> IO ()
buildAbc verbosity fs = do
    let pthreads = maybe "0" (\x -> if x then "1" else "0") $ lookup (FlagName "enable-pthreads") fs
    env <- getEnvironment
    rawSystemExitWithEnv verbosity "sh"
        (("scripts"</>"build-abc.sh") : (tail . words . show $ buildPlatform))
        ([("PTHREADS",pthreads)] ++ filter ((/="PTHREADS") . fst) env)
    createDirectoryIfMissingVerbose verbosity True static_dir
    copyFileVerbose verbosity ("abc-build"</>"libabc.a") (static_dir</>"libabc.a")
    onWindows $ copyFileVerbose verbosity ("abc-build"</>"libabc.dll") (static_dir</>"abc.dll")

-- Make sure the ABC libraries are installed in the appropriate places
postInstAbc :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postInstAbc _ flags pkg_descr lbi = do
    let copyflags = defaultCopyFlags {
                          copyDistPref  = installDistPref flags
                        , copyDest      = toFlag NoCopyDest
                        , copyVerbosity = installVerbosity flags
                        }
    postCopyAbc undefined copyflags pkg_descr lbi

postCopyAbc :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postCopyAbc _ flags pkg_descr lbi = do
    let installDirs = absoluteInstallDirs pkg_descr lbi
                . fromFlag . copyDest
                $ flags
        libPref = libdir installDirs
        binPref = bindir installDirs
        verbosity = fromFlag $ copyVerbosity flags
        outDir  = libPref
        copy dest f = installOrdinaryFile verbosity (static_dir</>f) (dest</>f)
    createDirectoryIfMissingVerbose verbosity True binPref
    copy libPref "libabc.a"
    onWindows $ copy libPref "abc.dll"
