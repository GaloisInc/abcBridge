#!/bin/sh
if [ -d abc-build -a -f abc-build/Makefile ]; then
  (cd abc-build; make clean)
fi
