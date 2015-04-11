{- |
Module      : Data.ABC.Internal.FRAIG
Copyright   : Galois, Inc. 2010-2014
License     : BSD3
Maintainer  : jhendrix@galois.com
Stability   : experimental
Portability : non-portable (c2hs, language extensions)

*Incomplete.* Binding of @sat\/fraig\/fraig.h@ for configuring the
process of generating functionally reduced AIGs.  Fraiging is the
special sauce that makes ABC outperform many vanilla SAT solvers.
-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Data.ABC.Internal.FRAIG
    ( Prove_Params_t_(..)
    , proveParamsDefault
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Foreign
import Foreign.C
import qualified System.IO.Unsafe as Unsafe

#include "abc.h"
#include "fraig.h"


{#pointer *Prove_Params_t -> Prove_Params_t_ #}

data Prove_Params_t_ = Prove_Params_t_
    {
    -- general parameters
      fUseFraiging'Prove_Params :: Bool          -- ^ enables fraiging
    , fUseRewriting'Prove_Params :: Bool         -- ^ enables rewriting
    , fUseBdds'Prove_Params :: Bool              -- ^ enables BDD construction when other methods fail
    , fVerbose'Prove_Params :: Bool              -- ^ prints verbose stats

    -- iterations
    , nItersMax'Prove_Params :: Int              -- ^ the number of iterations

    -- mitering
    , nMiteringLimitStart'Prove_Params :: Int    -- ^ starting mitering limit
    , nMiteringLimitMulti'Prove_Params :: Float  -- ^ multiplicative coefficient to increase the limit in each iteration

    -- rewriting
    , nRewritingLimitStart'Prove_Params :: Int   -- ^ the number of rewriting iterations
    , nRewritingLimitMulti'Prove_Params :: Float -- ^ multiplicative coefficient to increase the limit in each iteration

    -- fraiging
    , nFraigingLimitStart'Prove_Params :: Int    -- ^ starting backtrack(conflict) limit
    , nFraigingLimitMulti'Prove_Params :: Float  -- ^ multiplicative coefficient to increase the limit in each iteration

    -- last-gasp BDD construction
    , nBddSizeLimit'Prove_Params :: Int          -- ^ the number of BDD nodes when construction is aborted
    , fBddReorder'Prove_Params :: Bool           -- ^ enables dynamic BDD variable reordering

    -- last-gasp mitering
    , nMiteringLimitLast'Prove_Params :: Int     -- ^ final mitering limit

    -- global SAT solver limits
    , nTotalBacktrackLimit'Prove_Params :: Int64 -- ^ global limit on the number of backtracks
    , nTotalInspectLimit'Prove_Params :: Int64   -- ^ global limit on the number of clause inspects

    -- global resources applied
    , nTotalBacktracksMade'Prove_Params :: Int64 -- ^ the total number of backtracks made
    , nTotalInspectsMade'Prove_Params :: Int64   -- ^ the total number of inspects made
    } deriving (Read, Show, Eq)

instance Storable Prove_Params_t_ where
    sizeOf _ = {#sizeof Prove_Params_t #}
    alignment _ = 4 -- {#alignment Prove_Params_t #}
    peek p = Prove_Params_t_
        <$> liftM toBool ({#get Prove_Params_t->fUseFraiging #} p)
        <*> liftM toBool ({#get Prove_Params_t->fUseRewriting #} p)
        <*> liftM toBool ({#get Prove_Params_t->fUseBdds #} p)
        <*> liftM toBool ({#get Prove_Params_t->fVerbose #} p)
        <*> liftM fromIntegral ({#get Prove_Params_t->nItersMax #} p)
        <*> liftM fromIntegral ({#get Prove_Params_t->nMiteringLimitStart #} p)
        <*> liftM realToFrac ({#get Prove_Params_t->nMiteringLimitMulti #} p)
        <*> liftM fromIntegral ({#get Prove_Params_t->nRewritingLimitStart #} p)
        <*> liftM realToFrac ({#get Prove_Params_t->nRewritingLimitMulti #} p)
        <*> liftM fromIntegral ({#get Prove_Params_t->nFraigingLimitStart #} p)
        <*> liftM realToFrac ({#get Prove_Params_t->nFraigingLimitMulti #} p)
        <*> liftM fromIntegral ({#get Prove_Params_t->nBddSizeLimit #} p)
        <*> liftM toBool ({#get Prove_Params_t->fBddReorder #} p)
        <*> liftM fromIntegral ({#get Prove_Params_t->nMiteringLimitLast #} p)
        <*> liftM fromIntegral ({#get Prove_Params_t->nTotalBacktrackLimit #} p)
        <*> liftM fromIntegral ({#get Prove_Params_t->nTotalInspectLimit #} p)
        <*> liftM fromIntegral ({#get Prove_Params_t->nTotalBacktracksMade #} p)
        <*> liftM fromIntegral ({#get Prove_Params_t->nTotalInspectsMade #} p)
    poke p x = do
        {#set Prove_Params_t.fUseFraiging #}  p (fromBool $ fUseFraiging'Prove_Params x)
        {#set Prove_Params_t.fUseRewriting #}  p (fromBool $ fUseRewriting'Prove_Params x)
        {#set Prove_Params_t.fUseBdds #}  p (fromBool $ fUseBdds'Prove_Params x)
        {#set Prove_Params_t.fVerbose #}  p (fromBool $ fVerbose'Prove_Params x)
        {#set Prove_Params_t.nItersMax #}  p (fromIntegral $ nItersMax'Prove_Params x)
        {#set Prove_Params_t.nMiteringLimitStart #}  p (fromIntegral $ nMiteringLimitStart'Prove_Params x)
        {#set Prove_Params_t.nMiteringLimitMulti #}  p (realToFrac $ nMiteringLimitMulti'Prove_Params x)
        {#set Prove_Params_t.nRewritingLimitStart #}  p (fromIntegral $ nRewritingLimitStart'Prove_Params x)
        {#set Prove_Params_t.nRewritingLimitMulti #}  p (realToFrac $ nRewritingLimitMulti'Prove_Params x)
        {#set Prove_Params_t.nFraigingLimitStart #}  p (fromIntegral $ nFraigingLimitStart'Prove_Params x)
        {#set Prove_Params_t.nFraigingLimitMulti #}  p (realToFrac $ nFraigingLimitMulti'Prove_Params x)
        {#set Prove_Params_t.nBddSizeLimit #}  p (fromIntegral $ nBddSizeLimit'Prove_Params x)
        {#set Prove_Params_t.fBddReorder #}  p (fromBool $ fBddReorder'Prove_Params x)
        {#set Prove_Params_t.nMiteringLimitLast #}  p (fromIntegral $ nMiteringLimitLast'Prove_Params x)
        {#set Prove_Params_t.nTotalBacktrackLimit #}  p (fromIntegral $ nTotalBacktrackLimit'Prove_Params x)
        {#set Prove_Params_t.nTotalInspectLimit #}  p (fromIntegral $ nTotalInspectLimit'Prove_Params x)
        {#set Prove_Params_t.nTotalBacktracksMade #}  p (fromIntegral $ nTotalBacktracksMade'Prove_Params x)
        {#set Prove_Params_t.nTotalInspectsMade #}  p (fromIntegral $ nTotalInspectsMade'Prove_Params x)

-- fraigMan.c

{#fun Prove_ParamsSetDefault as ^
    { alloca- `Prove_Params_t_' peek*
    } -> `()' #}


-- a more appropriate name in a convenient form
proveParamsDefault :: Prove_Params_t_
proveParamsDefault = Unsafe.unsafePerformIO proveParamsSetDefault
