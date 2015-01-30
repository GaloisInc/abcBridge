{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

{- |
Module      : Data.ABC.Internal.ABCGlobal
Copyright   : (c) Galois, Inc. 2010
License     : BSD3
Maintainer  : jhendrix@galois.com
Stability   : experimental
Portability : not portable (c2hs, language extensions)

/Incomplete./  Binding of @misc\/util\/abc_global.h@ which contains
miscellaneous functions for ABC, including a counterexample datastructure
and error handling mechanisms.

-}

module Data.ABC.Internal.ABCGlobal (
    -- * Counterexamples
      Abc_Cex_t_(..)
    , Abc_Cex_t
    , peekAbcCex
    ) where

#include <stdio.h>
#include "abc_global.h"
#include "abcbridge.h"

import Data.Word
import Control.Applicative
import Control.Exception (assert)
import Foreign
import Foreign.C

divUp :: Integral a => a -> a -> a
divUp a b = let (d,r) = divMod a b
            in if r == 0 then d else succ d

-- | @chunksOf n l@ partitions l into length @n@ chunks.  The last chunk
-- may contain fewer than @n@ elements.
chunksOf :: Int -> [e] -> [[e]]
chunksOf n = assert (n > 0) go
  where go [] = []
        go l = h : go r
          where (h,r) = splitAt n l


data Abc_Cex_t_ = Abc_Cex_t_
    { iPo'Abc_Cex :: Int    -- ^ the zero-based number of PO, for which verification failed
    , iFrame'Abc_Cex :: Int -- ^ the zero-based number of the time-frame, for which verificaiton failed
    , nRegs'Abc_Cex  :: Int -- ^ the number of registers in the miter
    , nPis'Abc_Cex   :: Int -- ^ the number of primary inputs in the miter
    , nBits'Abc_Cex  :: Int -- ^ the number of words of bit data used (ezyang: where by words they actually mean bits)

    -- | The cex bit data (the number of bits: @nRegs + (iFrame+1) * nPis@)
    -- The format of the data is as such:
    --
    --    * First, the initial values for all registers
    --
    --    * Then, the @iFrame+1@ sets of inputs, which represent what we
    --      inputted into the network at each timestep.  For a
    --      combinational network, this means there is only 1 set.
    , pData'regs'Abc_Cex :: [Bool]
    , pData'inputs'Abc_Cex :: [[Bool]] -- ^ outer length: @iFrame+1@; inner length: @nPis@
    } deriving (Read, Show, Eq)

{#pointer *Abc_Cex_t -> Abc_Cex_t_ #}
-- Note that we can't write a Storable instance because Abc_Cex_t doesn't have
-- a fixed size.

-- | Peek into the value of a Abc_Cex_t.
peekAbcCex :: Abc_Cex_t -> IO Abc_Cex_t_
peekAbcCex p = do
    iPo     <- fromIntegral <$> {#get Abc_Cex_t->iPo #} p
    iFrame  <- fromIntegral <$> {#get Abc_Cex_t->iFrame #} p
    nRegs   <- fromIntegral <$> {#get Abc_Cex_t->nRegs #} p
    nPis    <- fromIntegral <$> {#get Abc_Cex_t->nPis #} p
    nBits   <- fromIntegral <$> {#get Abc_Cex_t->nBits #} p

    let size = 32 -- this is what the assume in the code!

    -- read out the data (it's a big bit field, so we'll
    -- convert it into a flat [Bool] before slicing and dicing);
    -- there might be a little extra left over
    ws <- peekArray (nBits `divUp` size)
                    (castPtr (plusPtr p {#sizeof Abc_Cex_t#}) :: Ptr Word32)
                    :: IO [Word32]

    let bits = concatMap (\x -> map (testBit x) [0..size-1]) ws :: [Bool]
        -- slice it up
        (regs, flatInputs) = splitAt nRegs bits
        inputs = chunksOf nPis ((take ((iFrame + 1) * nPis)) flatInputs)
    return $ Abc_Cex_t_ iPo iFrame nRegs nPis nBits regs inputs
