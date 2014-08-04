{-# LANGUAGE ForeignFunctionInterface #-}
{- |
Module      : Data.ABC.Internal.Orphan
Copyright   : Galois, Inc. 2010
License     : BSD3
Maintainer  : jhendrix@galois.com
Stability   : experimental
Portability : non-portable (language extensions)

/Incomplete./ Binding of various orphan functions in ABC:
functions that are frequently extern'ed into scope but are not
defined in any header; we've created a new header file @orphan.h@
to accomodate these.

-}

module Data.ABC.Internal.Orphan
    ( abcNtkFromAigPhase
    -- * base\/abci\/abcDar.c
    , abcNtkToDar
    ) where

import Foreign (fromBool)
import Foreign.C

{#import Data.ABC.Internal.ABC#}
{#import Data.ABC.Internal.AIG#}

#include "abc.h"
#include "fra.h"
#include "orphan.h"

abcNtkToDar :: Abc_Ntk_t -> Bool -> Bool -> IO Aig_Man_t
abcNtkToDar ntk exors rl =
  abcNtkToDar' ntk (fromBool exors) (fromBool rl)

foreign import ccall unsafe "Abc_NtkToDar"
  abcNtkToDar' :: Abc_Ntk_t -> CInt -> CInt -> IO Aig_Man_t

{#fun Abc_NtkFromAigPhase as ^ { `Aig_Man_t' } -> `Abc_Ntk_t' #}
