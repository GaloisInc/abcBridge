{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

{- |
Module      : Data.ABC.Internal.CEC
Copyright   : Galois, Inc. 2010-2014
License     : BSD3
Maintainer  : jhendrix@galois.com
Stability   : experimental
Portability : non-portable (c2hs, language extensions)

Comprehensive binding of @aig\/cec\/cec.h@ for performing combinational
equivalence checking of scalable and-inverter graphs (GIA).

-}

module Data.ABC.Internal.CEC (
    -- * Types
    -- ** Storable types
      Cec_ParCec_t_(..)
    , Cec_ParSat_t_(..)
    -- * Functions
    -- ** cecCec.c
    , cecManVerify
    -- ** cecCore.c
    , cecManSatDefaultParams
    , cecManCecDefaultParams
    , cecManSatSolving
    , Cec_ManPat_t_
    , Cec_ManPat_t
    , cecManPatStart
    , cecManPatStop
    , cecManPatPatCount
    , cecManPatPrintStats
    , cecManSatSolve
    ) where

#include "gia.h"
#include "cec.h"
#include "cecInt.h"

import Control.Applicative
import Foreign
import Foreign.C (CInt(..))
import qualified System.IO.Unsafe as Unsafe

{#import Data.ABC.Internal.GIA#}

{#pointer *Cec_ParSat_t -> Cec_ParSat_t_ #}
type Cec_ParCec_t = Ptr Cec_ParCec_t_

data Cec_ManPat_t_
{#pointer *Cec_ManPat_t -> Cec_ManPat_t_ #}

data Cec_ParSat_t_ = Cec_ParSat_t_
     { nBTLimit'Cec_ParSat      :: CInt
     , nSatVarMax'Cec_ParSat    :: CInt
     , nCallsRecycle'Cec_ParSat :: CInt
     , fNonChrono'Cec_ParSat  :: Bool
     , fPolarFlip'Cec_ParSat  :: Bool
     , fCheckMiter'Cec_ParSat :: Bool
     , fLearnCls'Cec_ParSat   :: Bool
     , fVerbose'Cec_ParSat    :: Bool
     } deriving (Show, Read, Eq)

instance Storable Cec_ParSat_t_ where
  sizeOf _ = {#sizeof Cec_ParSat_t #}
  alignment _ = alignment (undefined :: CInt)
  peek p = Cec_ParSat_t_
      <$> pInt  {#get Cec_ParSat_t->nBTLimit      #}
      <*> pInt  {#get Cec_ParSat_t->nSatVarMax    #}
      <*> pInt  {#get Cec_ParSat_t->nCallsRecycle #}
      <*> pBool {#get Cec_ParSat_t->fNonChrono  #}
      <*> pBool {#get Cec_ParSat_t->fPolarFlip  #}
      <*> pBool {#get Cec_ParSat_t->fCheckMiter #}
      <*> pBool {#get Cec_ParSat_t->fLearnCls #}
      <*> pBool {#get Cec_ParSat_t->fVerbose  #}
    where pInt  :: (Cec_ParSat_t -> IO CInt) -> IO CInt
          pInt  f = f p
          pBool :: (Cec_ParSat_t -> IO CInt) -> IO Bool
          pBool f = (/= 0) <$> f p
  poke p x = do
      pokeInt  {#set Cec_ParSat_t->nBTLimit      #} nBTLimit'Cec_ParSat
      pokeInt  {#set Cec_ParSat_t->nSatVarMax    #} nSatVarMax'Cec_ParSat
      pokeInt  {#set Cec_ParSat_t->nCallsRecycle #} nCallsRecycle'Cec_ParSat
      pokeBool {#set Cec_ParSat_t->fNonChrono  #}   fNonChrono'Cec_ParSat
      pokeBool {#set Cec_ParSat_t->fPolarFlip  #}   fPolarFlip'Cec_ParSat
      pokeBool {#set Cec_ParSat_t->fCheckMiter #}   fCheckMiter'Cec_ParSat
      pokeBool {#set Cec_ParSat_t->fLearnCls #}     fLearnCls'Cec_ParSat
      pokeBool {#set Cec_ParSat_t->fVerbose  #}     fVerbose'Cec_ParSat
    where pokeInt :: (Cec_ParSat_t -> CInt -> IO ()) -> (Cec_ParSat_t_ -> CInt) -> IO ()
          pokeInt w f = w p (f x)
          pokeBool :: (Cec_ParSat_t -> CInt -> IO ()) -> (Cec_ParSat_t_ -> Bool) -> IO ()
          pokeBool w f = w p (if f x then 1 else 0)

data Cec_ParCec_t_ = Cec_ParCec_t_
    { nBTLimit'Cec_ParCec :: Int      -- ^ conflict limit at a node
    , nTimeLimit'Cec_ParCec :: Int    -- ^ the runtime limit in seconds (added prefix @n@)
    , fUseSmartCnf'Cec_ParCec :: Bool -- ^ use smart CNF computation
    , fRewriting'Cec_ParCec :: Bool   -- ^ enables AIG rewriting
    , fVeryVerbose'Cec_ParCec :: Bool -- ^ very verbose stats
    , fVerbose'Cec_ParCec :: Bool     -- ^ verbose stats
    , iOutFail'Cec_ParCec :: Int      -- ^ the number of failed output
    } deriving (Show, Read, Eq)

instance Storable Cec_ParCec_t_ where
    sizeOf _ = {#sizeof Cec_ParCec_t #}
    alignment _ = alignment (undefined :: CInt)
    peek p = Cec_ParCec_t_
        <$> fmap fromIntegral ({#get Cec_ParCec_t->nBTLimit #} p)
        <*> fmap fromIntegral ({#get Cec_ParCec_t->TimeLimit #} p)
        <*> fmap toBool ({#get Cec_ParCec_t->fUseSmartCnf #} p)
        <*> fmap toBool ({#get Cec_ParCec_t->fRewriting #} p)
        <*> fmap toBool ({#get Cec_ParCec_t->fVeryVerbose #} p)
        <*> fmap toBool ({#get Cec_ParCec_t->fVerbose #} p)
        <*> fmap fromIntegral ({#get Cec_ParCec_t->iOutFail #} p)
    poke p x = do
        {#set Cec_ParCec_t.nBTLimit #}      p (fromIntegral $ nBTLimit'Cec_ParCec x)
        {#set Cec_ParCec_t.TimeLimit #}     p (fromIntegral $ nTimeLimit'Cec_ParCec x)
        {#set Cec_ParCec_t.fUseSmartCnf #}  p (fromBool $ fUseSmartCnf'Cec_ParCec x)
        {#set Cec_ParCec_t.fRewriting #}    p (fromBool $ fRewriting'Cec_ParCec x)
        {#set Cec_ParCec_t.fVeryVerbose #}  p (fromBool $ fVeryVerbose'Cec_ParCec x)
        {#set Cec_ParCec_t.fVerbose #}      p (fromBool $ fVerbose'Cec_ParCec x)
        {#set Cec_ParCec_t.iOutFail #}      p (fromIntegral $ iOutFail'Cec_ParCec x)


-- cecCore.c

{-# NOINLINE cecManSatDefaultParams #-}
cecManSatDefaultParams :: Cec_ParSat_t_
cecManSatDefaultParams = Unsafe.unsafePerformIO $ do
  alloca $ \p -> do
    cecManSatSetDefaultParams p
    peek p

foreign import ccall unsafe "Cec_ManSatSetDefaultParams"
  cecManSatSetDefaultParams :: Cec_ParSat_t -> IO ()

{-# NOINLINE cecManCecDefaultParams #-}
cecManCecDefaultParams :: Cec_ParCec_t_
cecManCecDefaultParams = Unsafe.unsafePerformIO $ do
  alloca $ \p -> do
    cecManCecSetDefaultParams p
    peek p

foreign import ccall unsafe "Cec_ManCecSetDefaultParams"
  cecManCecSetDefaultParams :: Cec_ParCec_t -> IO ()

{#fun Cec_ManPatStart as ^ { } -> `Cec_ManPat_t' #}

{#fun Cec_ManPatStop as ^ { `Cec_ManPat_t' } -> `()' #}

{#fun Cec_ManPatPrintStats as ^ { `Cec_ManPat_t' } -> `()' #}

cecManPatPatCount :: Cec_ManPat_t -> IO CInt
cecManPatPatCount = {#get Cec_ManPat_t->nPats#}

{#fun Cec_ManSatSolve as ^
    { `Cec_ManPat_t' -- pPat
    , id `Gia_Man_t' -- pAig
    , `Cec_ParSat_t' -- pPars
    } -> `()' #}


cecManSatSolving :: Gia_Man_t -> Cec_ParSat_t_ -> IO Gia_Man_t
cecManSatSolving aig pars = do
  with pars $ \p -> cecManSatSolving' aig p

foreign import ccall unsafe "Cec_ManSatSolving"
  cecManSatSolving' :: Gia_Man_t -> Cec_ParSat_t -> IO Gia_Man_t

cecManVerify :: Gia_Man_t -> Cec_ParCec_t_ -> IO CInt
cecManVerify m pars = with pars $ \p -> cecManVerify' m p

foreign import ccall unsafe "Cec_ManVerify"
  cecManVerify' :: Gia_Man_t -> Cec_ParCec_t -> IO CInt
