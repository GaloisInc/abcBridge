#!/bin/sh

# bail out immediately if any command fails
set -e

ARCH="$1"
OS="$2"

SRC_TARBALL="https://bitbucket.org/rdockins/abc/get/galois-abcBridge.tar.bz2"
LOCAL_TARBALL="galois-abcBridge.tar.bz2"

echo "Setting up ABC tree for abcBridge version ${PACKAGE_VERSION}..."

# If the ABC source is not already fetched, download the galois-abcBridge
# branch of the ABC project and unpack it in the "abc-build" subdirectory
if [ ! -d abc-build ]; then
  # Fetch the latest galois-abcBridge branch from BitBucket; use either curl or wget
  # depending on which is installed
  [ -e $LOCAL_TARBALL ] || curl -O $SRC_TARBALL || wget --no-check-certificate $SRC_TARBALL

  # Unpack into the abc-build subdirectory
  # Note: some games are played to strip off the top-level directory name that
  # is automatically assigned by BitBucket
  mkdir -p abc-build && (cd abc-build; tar xfj "../$LOCAL_TARBALL" --strip-components=1)
fi

# Interrogate the expected version number of the ABC sources
if [ -e abc-build/galois-abcBridge.version ]; then
  ABC_VERSION=`cat abc-build/galois-abcBridge.version`
else
  ABC_VERSION="NONE"
fi

if [ $ABC_VERSION != $PACKAGE_VERSION ]; then
  echo ""
  echo "The ABC source version ${ABC_VERSION} does not match the abcBridge package version version ${PACKAGE_VERSION}."
  echo ""
  echo "This may cause problems with the build; it is recommended you cancel this build and check out matching ABC sources"
  echo "into the abc-build directory and try again.  You can run scripts/clean_abc.sh to remove the current ABC sources;"
  echo "this will cause latest development branch to be automatically downloaded on the next build."
  echo ""
  echo "Press [enter] to continue anyway, or press [^C] to abort (build will automatically abort in 15 seconds)."
  read -t 15
fi

# Build a list of the files in the ABC subdirectory that we can feed into
# the Cabal system so that "setup sdist" works correctly.  Use sed to filter out
# compiled object files and libraries. Likewise, set up a list of directories
# so we can include relevant *.h files
#
# Note: fully-qualified 'find' is referenced to work around a problem building
# under MinGW where unqualified 'find' refers to the Win32 utility of the same name

if [ ! -e abc-build/abc-incl-dirs.txt ]; then
  /usr/bin/find abc-build/src -type d > abc-build/abc-incl-dirs.txt
fi

if [ ! -e abc-build/abc-sources.txt ]; then
  # touch the listing file we are about to produce so that it will appear in the file listing!
  touch abc-build/abc-sources.txt
  /usr/bin/find abc-build -type f | sed -e '/\/\.hg\//d' -e '/\.hgignore$/d' -e '/\.o$/d' -e '/\.a$/d' -e '/\.dll$/d' -e '/\.lib$/d' > abc-build/abc-sources.txt
fi


# Make sure the build scripts are executable
chmod +x abc-build/depends.sh
