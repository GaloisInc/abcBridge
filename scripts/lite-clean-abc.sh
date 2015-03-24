#!/bin/sh
if [ -d abc-build ]; then
  (cd abc-build; make clean)
fi
