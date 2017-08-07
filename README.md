[![Build
Status](https://travis-ci.org/GaloisInc/abcBridge.svg?branch=master)](https://travis-ci.org/GaloisInc/abcBridge)

# abcBridge: Haskell bindings for ABC

Directory structure:

    src/        Haskell bindings source tree.
    cbits/      C source tree for bindings.
    include/    Header files used.
    tests/      Test scripts.
    scripts/	Build system scripts.

## Building

We have attempted to make `cabal install` work out of the box.  They
have been tested on 64-bit Mac OS X with XCode, 32-bit and 64-bit CentOS
Linux 5.x and 6.x, and 32-bit and 64-bit Windows XP with MinGW. On Windows,
the 64-bit build is less well tested than the 32-bit build, so
try a 32-bit build if you have trouble with the 64-bit build.


## ABC Sources

The main task of the build system is to retrieve and build the ABC C
sources before building the Haskell bridge.  We do this by downloading
the latest version of a branch of the ABC project from BitBucket.  See
"scripts/setup-abc.sh" for the logic relating to this step, including
the URL from which the sources are fetched.  If you run "cabal sdist",
ABC sources will be fetched and unpacked, and then repacked into a
combined source distribution with the Haskell code.  The resulting
sdist tarball can be compiled without needing to fetch ABC sources
from BitBucket.

The task of building the ABC sources is performed by the
"scripts/build-abc.sh".  It mostly involves selecting some compiler
flags depending on the arch and OS and then invoking the ABC makefile.

Run "scripts/clean-abc.sh" if you want to remove all ABC source
artifacts and force a redownload from the BitBucket project.


## Cutting a Hackage release

See README.releases for instructions.
