#!/bin/bash

# bail out immediately if any command fails
set -e

ARCH="$1"
OS="$2"

# Select architecture- and OS-dependent compiler flags
case "$OS" in
  "Linux")
    case "$ARCH" in
      "I386") A="-m32 -DLIN -DSIZEOF_VOID_P=4 -DSIZEOF_LONG=4 -DSIZEOF_INT=4 -static-libgcc" ;;
      "X86_64") A="-m64 -fPIC -DLIN64 -DSIZEOF_VOID_P=8 -DSIZEOF_LONG=8 -DSIZEOF_INT=4 -static-libgcc" ;;
      *) echo "Unknown architecture: $ARCH" ; exit 2 ;;
    esac
    NPROC=$(nproc) ;;

  "OSX")
    case "$ARCH" in
      "I386") A="-m32 -DLIN -DSIZEOF_VOID_P=4 -DSIZEOF_LONG=4 -DSIZEOF_INT=4" ;;
      "X86_64") A="-m64 -fPIC -DLIN64 -DSIZEOF_VOID_P=8 -DSIZEOF_LONG=8 -DSIZEOF_INT=4" ;;
      "AArch64") A="-m64 -fPIC -DLIN64 -DSIZEOF_VOID_P=8 -DSIZEOF_LONG=8 -DSIZEOF_INT=4" ;;
      *) echo "Unknown architecture: $ARCH" ; exit 2 ;;
    esac
    NPROC=$(sysctl -n hw.ncpu) ;;

  "Windows")
    case "$ARCH" in
      "I386")
        A="-m32 \
           -DWIN32_NO_DLL \
           -DABC_NO_DYNAMIC_LINKING \
           -DLIN \
           -DNT \
           -D_WIN32 \
           -UWIN32 \
           -DSIZEOF_VOID_P=4 -DSIZEOF_LONG=4 -DSIZEOF_INT=4 \
           -DPTW32_STATIC_LIB \
           -UZLIB_DLL \
           -Wl,--undefined=___strtod,--wrap,strtod,--defsym,___wrap_strtod=___strtod" ;;

      "X86_64")
        # The `SIZEOF_*` arguments came from printing the
        # corresponding `sizeof` results (using
        # `abc-build/arch_flags.c`) on the Win64 machine. Note that
        # the `SIZEOF_LONG` differs from the Lin64 value.
        A="-m64 \
           -DWIN32_NO_DLL \
           -DABC_NO_DYNAMIC_LINKING \
           -DNT64 \
           -D_WIN64 \
           -UWIN32 \
           -DSIZEOF_VOID_P=8 -DSIZEOF_LONG=4 -DSIZEOF_INT=4 \
           -UZLIB_DLL" ;;
      *) echo "Unknown architecture: $ARCH" ; exit 2 ;;
    esac
    if [ ! -z "${NUMBER_OF_PROCESSORS:-}" ]; then
      NPROC="$NUMBER_OF_PROCESSORS"
    elif type -p nproc; then
      NPROC=$(nproc)
    else
      NPROC="1"
    fi ;;

  *) echo "Unknown OS: $OS" ; exit 2 ;;
esac

cd abc-build
make -j$NPROC ARCHFLAGS="-DABC_LIB $A" ABC_USE_NO_PTHREADS=1 ABC_USE_NO_READLINE=1 libabc.a abc
