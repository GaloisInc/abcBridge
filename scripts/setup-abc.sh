#!/bin/sh

# bail out immediately if any command fails
set -e

echo "Setting up ABC tree for abcBridge version ${PACKAGE_VERSION:-undefined}..."

which tar
tar --version

if [ -z "${PACKAGE_VERSION}" ]; then
  if [ -d abc-build ]; then
    echo ""
    echo "Package version not defined; assuming compatible ABC sources are already present in directory abc-build"
  else
    echo ""
    echo "Package version not defined.  Expected to find ABC sources in directory abc-build, but they are missing."
    echo "Please manually check out or download ABC sources into directory 'abc-build'.  Alternately, execute 'cabal configure'"
    echo "instead to automatically fetch the correct sources."
    exit 1
  fi
else
  LOCAL_TAR="galois-abcBridge-${PACKAGE_VERSION}.tar"
  LOCAL_TARBALL="${LOCAL_TAR}.gz"
  SRC_TARBALL="https://bitbucket.org/rdockins/abc/get/${LOCAL_TARBALL}"
  SUCCESS=""

  # try at most twice to fetch sources...
  for i in "one" "two"
  do
      # If the ABC source is not already fetched, download the galois-abcBridge
      # branch of the ABC project and unpack it in the "abc-build" subdirectory
      if [ ! -d abc-build ]; then
	  # Fetch the latest galois-abcBridge branch from BitBucket; use either curl or wget
	  # depending on which is installed
	  [ -e $LOCAL_TARBALL ] || curl -O $SRC_TARBALL || wget --no-check-certificate $SRC_TARBALL

	  # Unpack into the abc-build subdirectory
	  # Note: some games are played to strip off the top-level directory name that
	  # is automatically assigned by BitBucket
	  mkdir -p abc-build && (cd abc-build; cat "../$LOCAL_TARBALL" | tar -x -z --strip-components=1)
      fi

      # Interrogate the expected version number of the ABC sources
      if [ -e abc-build/galois-abcBridge.version ]; then
	  ABC_VERSION=`cat abc-build/galois-abcBridge.version`
      else
	  ABC_VERSION="NONE"
      fi

      if [ "$ABC_VERSION" != "$PACKAGE_VERSION" ]; then
	  echo ""
	  echo "The ABC source version $ABC_VERSION does not match the abcBridge package version $PACKAGE_VERSION."
	  echo ""
	  echo "Attempting to clean up and fetch fresh sources..."

	  rm -r abc-build     || true
	  rm "$LOCAL_TARBALL" || true
      else
	  echo "ABC sources found"
	  SUCCESS="success"
	  break
      fi
  done

  if [ -z "${SUCCESS}" ]; then
      echo ""
      echo "Unable to fetch ABC sources. Giving up..."
      exit 1
  fi
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
