#!/bin/sh

# bail out immediately if any command fails
set -e

ARCH="$1"
OS="$2"

# Default to do nothing for the REMOVE_CMD
REMOVE_CMD=true

REMOVE_ARGS="-R .drectve \
  -R .debug \
  -R .debug_abbrev \
  -R .debug_aranges \
  -R .debug_frame \
  -R .debug_info \
  -R .debug_line \
  -R .debug_loc \
  -R .debug_pubnames \
  -R .debug_pubtypes \
  -R .debug_ranges \
  -R .debug_str"

# Select architecture-dependent compiler flags
case "$ARCH" in
  "I386") A="-m32 -fPIC -DLIN -DSIZEOF_VOID_P=4 -DSIZEOF_LONG=4 -DSIZEOF_INT=4" ;;
  "X86_64") A="-m64 -fPIC -DLIN64 -DSIZEOF_VOID_P=8 -DSIZEOF_LONG=8 -DSIZEOF_INT=4" ;;
  *) echo "Unknown architecture: $ARCH" ; exit 2 ;;
esac

# Set up some OS-dependent flags and actions
case "$OS" in
  "Linux") S="" ;;
  "OSX") S="" ;;
  "Windows") S="libabc.dll" ; A="-m32 -DWIN32_NO_DLL -DABC_NO_DYNAMIC_LINKING -DLIN -DNT -UWIN32 -DSIZEOF_VOID_P=4 -DSIZEOF_LONG=4 -DSIZEOF_INT=4 -DPTW32_STATIC_LIB -UZLIB_DLL -lmsvcrt -Wl,--undefined=___strtod,--wrap,strtod,--defsym,___wrap_strtod=___strtod" ; REMOVE_CMD="objcopy ${REMOVE_ARGS}" ;;
  *) echo "Unknown OS: $OS" ; exit 2 ;;
esac

# Default to no pthreads
if [ -z "$PTHREADS" ]; then
  PTHREADS=0
fi

cd abc-build
make -j4 ARCHFLAGS="-DABC_LIB $A" REMOVE_DRECTVE="$REMOVE_CMD" READLINE=0 PTHREADS="$PTHREADS" libabc.a $S

