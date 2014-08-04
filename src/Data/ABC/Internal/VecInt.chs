{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

{- |
Module      : Data.ABC.Internal.VecInt
Copyright   : Galois, Inc. 2010
License     : BSD3
Maintainer  : jhendrix@galois.com
Stability   : experimental
Portability : non-portable (c2hs, language extensions)

/Incomplete./ Binding of @misc\/vec\/vecInt.h@ for manipulating
vectors of integers.

-}

module Data.ABC.Internal.VecInt (
      Vec_Int_t_
    , Vec_Int_t
    , clearVecInt
    , vecIntSize , setVecIntSize
    , vecIntCap  , setVecIntCap
    , vecIntArray, setVecIntArray
    , vecIntEntry
    , vecIntWriteEntry
    , withVecInt
    ) where

#include <stdio.h>
#include "vec.h"
#include "vecInt.h"

import Control.Exception (bracket)
import Foreign.C
import Foreign
import Data.ABC.Internal.Field

data Vec_Int_t_

{#pointer *Vec_Int_t -> Vec_Int_t_ #}

vecIntSizeField :: Field Vec_Int_t CInt
vecIntSizeField = fieldFromOffset {#offsetof Vec_Int_t->nSize #}

vecIntSize :: Vec_Int_t -> IO CInt
vecIntSize = readAt vecIntSizeField

setVecIntSize :: Vec_Int_t -> CInt -> IO ()
setVecIntSize = {#set Vec_Int_t->nSize #}

vecIntCap :: Vec_Int_t -> IO CInt
vecIntCap = {#get Vec_Int_t->nCap #}

setVecIntCap :: Vec_Int_t -> CInt -> IO ()
setVecIntCap = {#set Vec_Int_t->nCap #}

clearVecInt :: Vec_Int_t -> IO ()
clearVecInt v = writeAt vecIntSizeField v 0

vecIntArray :: Vec_Int_t -> IO (Ptr CInt)
vecIntArray = {#get Vec_Int_t->pArray #}

setVecIntArray :: Vec_Int_t -> Ptr CInt -> IO ()
setVecIntArray = {#set Vec_Int_t->pArray #}

-- | Get entry in vector at given index.
vecIntEntry :: Vec_Int_t -> CInt -> IO CInt
vecIntEntry v i = do
  a <- vecIntArray v
  peekElemOff a (fromIntegral i)

-- | Write entry in vector at given index.
vecIntWriteEntry :: Vec_Int_t -> CInt -> CInt -> IO ()
vecIntWriteEntry v i u = do
  a <- vecIntArray v
  pokeElemOff a (fromIntegral i) u

withVecInt :: CInt -> Ptr CInt -> (Vec_Int_t -> IO a) -> IO a
withVecInt sz p act = do
  bracket (mallocBytes {#sizeof Vec_Int_t #}) free $ \v -> do
    setVecIntSize v sz
    setVecIntCap  v sz
    setVecIntArray v p
    act v
