#!/bin/sh

# bail out immediately if any command fails
set -e

# trace execution
set -x

# Build a list of the files in the ABC subdirectory that we can feed into
# the Cabal system so that "setup sdist" works correctly.  Use sed to filter out
# compiled object files and libraries. Likewise, set up a list of directories
# so we can include relevant *.h files
#
# Note: fully-qualified 'find' is referenced to work around a problem building
# under MinGW where unqualified 'find' refers to the Win32 utility of the same name

if [ ! -e scripts/abc-incl-dirs.txt ]; then
  /usr/bin/find abc-build/src -type d > scripts/abc-incl-dirs.txt
fi

if [ ! -e scripts/abc-sources.txt ]; then
  # touch the listing file we are about to produce so that it will appear in the file listing!
  touch scripts/abc-sources.txt
  /usr/bin/find abc-build -type f | sed -e '/\/\.hg\//d' -e '/\.hgignore$/d' -e '/\.o$/d' -e '/\.a$/d' -e '/\.dll$/d' -e '/\.lib$/d' > scripts/abc-sources.txt
fi

# Make sure the build scripts are executable
chmod +x abc-build/depends.sh
