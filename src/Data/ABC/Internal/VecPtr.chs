{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

{- |
Module      : Data.ABC.Internal.VecPtr
Copyright   : Galois, Inc. 2010
License     : BSD3
Maintainer  : jhendrix@galois.com
Stability   : experimental
Portability : non-portable (c2hs, language extensions)

/Incomplete./ Binding of @misc\/vec\/vecPtr.h@ for manipulating
vectors of pointers.

-}

module Data.ABC.Internal.VecPtr (
      Vec_Ptr_t_
    , Vec_Ptr_t
    , clearVec
    , vecPtrSize
    , vecPtrArray
    , vecPtrEntry
    ) where

#include <stdio.h>
#include "vec.h"
#include "vecPtr.h"

import Control.Exception (assert)
import Foreign
import Foreign.C

import Data.ABC.Internal.Field

data Vec_Ptr_t_

{#pointer *Vec_Ptr_t -> Vec_Ptr_t_ #}

vecSizeField :: Field Vec_Ptr_t CInt
vecSizeField = fieldFromOffset {#offsetof Vec_Ptr_t->nSize #}

clearVec :: Vec_Ptr_t -> IO ()
clearVec v = writeAt vecSizeField v 0

vecPtrSize :: Vec_Ptr_t -> IO Int
vecPtrSize v = fromIntegral `fmap` readAt vecSizeField v

vecPtrArray :: Vec_Ptr_t -> IO (Ptr (Ptr a))
vecPtrArray v = castPtr `fmap` {#get Vec_Ptr_t->pArray #} v

vecPtrEntry :: Vec_Ptr_t -> Int -> IO (Ptr a)
vecPtrEntry v i = do
  assert (i >= 0) $ return ()
  do sz <- vecPtrSize v
     assert (i < sz) $ return ()
  a <- vecPtrArray v
  peekElemOff a i

