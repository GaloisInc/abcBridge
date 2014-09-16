#!/bin/sh

# bail out immediately if any command fails
set -e

ARCH="$1"
OS="$2"

SRC_TARBALL="https://bitbucket.org/rdockins/abc/get/galois-abcBridge.tar.bz2"
LOCAL_TARBALL="galois-abcBridge.tar.bz2"

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

# Build a list of the files in the ABC subdirectory that we can feed into
# the Cabal system so that "setup sdist" works correctly.  Use sed to filter out
# compiled object files and libraries. Likewise, set up a list of directorie
# so we can include relevant *.h files
#
# Note: fully-qualified 'find' is referenced to work around a problem building
# under MinGW where unqualified 'find' refers to the Win32 utility of the same name

if [ ! -e scripts/abc-sources.txt ]; then
  /usr/bin/find abc-build -type f | sed -e '/\/\.hg\//d' -e '/\.hgignore$/d' -e '/\.o$/d' -e '/\.a$/d' -e '/\.dll$/d' -e '/\.lib$/d' > scripts/abc-sources.txt
fi

if [ ! -e scripts/abc-incl-dirs.txt ]; then
  /usr/bin/find abc-build/src -type d > scripts/abc-incl-dirs.txt
fi

# Make sure the build scripts are executable
chmod +x abc-build/depends.sh
