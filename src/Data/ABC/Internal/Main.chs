{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

{- |
Module      : Data.ABC.Internal.Main
Copyright   : Galois, Inc. 2010
License     : BSD3
Maintainer  : jhendrix@galois.com
Stability   : experimental
Portability : non-portable (c2hs, language extensions)

/Incomplete./ Binding of @base\/main\/main.h@ for managing
the global state of the ABC library.
-}

module Data.ABC.Internal.Main (
    -- * main.c
      abcStart
    , abcStop
    ) where

#include "base/main/main.h"

{#fun Abc_Start as ^ {} -> `()' #}
{#fun Abc_Stop as ^ {} -> `()' #}
