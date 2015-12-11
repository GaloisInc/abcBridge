{- |
Module      : Data.ABC
Copyright   : (c) Galois, Inc. 2010-2014
License     : BSD3
Maintainer  : jhendrix@galois.com
Stability   : experimental
Portability : portable

Contains main interface to ABC, a system for sequential synthesis
and verification.

ABC provides many functions for manipulating Boolean networks.
Internally, ABC provides two different ways of representing them: the
older "AIG" interface, and a newer "GIA" interface.  This library
exposes both interfaces, along with a handful of functions for
manipulating them.

-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.ABC (
      -- * Library setup and teardown
      initialize
    , unsafeCleanup

      -- * Standard ABC interface
    , AIG
    , AIGLit
    , aigNetwork
    , AIG.newAIG
    , readAigerAsAIG

      -- * New style ABC representation
    , GIA
    , GIALit
    , giaNetwork
    , GIA.newGIA
    , readAigerAsGIA

    , module Data.AIG
    ) where

import Data.ABC.Internal.Main

import Data.AIG

import Data.ABC.AIG (AIG)
import qualified Data.ABC.AIG as AIG

import Data.ABC.GIA (GIA)
import qualified Data.ABC.GIA as GIA

import Prelude hiding (and, not)

type AIGLit = AIG.Lit

{-# WARNING aigNetwork, readAigerAsAIG
    "The Data.ABC.AIG module has known bugs (http://github.com/GaloisInc/abcBridge/issues/4) for which solutions do not currently exist.  Consider using Data.ABC.GIA instead."
  #-}

-- | Proxy for AIG interface.
aigNetwork :: Proxy AIGLit AIG
aigNetwork = AIG.proxy

type GIALit = GIA.Lit

-- | Proxy for GIA interface.
giaNetwork :: Proxy GIALit GIA
giaNetwork = GIA.proxy

-- | Initializes the ABC engine.  This function may be safely called
-- multiple times.  Higher-level functions will automatically call this
-- function, so it is only needed if using the FFI interfaces directly.
initialize :: IO ()
initialize = abcStart

-- | Deinitializes the ABC engine.  ABC operations may not be run after
-- this function is called.  Use with care; this may cause ABC datatypes
-- to stop working.
unsafeCleanup :: IO ()
unsafeCleanup = abcStop


-- | Read an AIGER file as an AIG network.
readAigerAsAIG :: FilePath -> IO (Network AIGLit AIG)
readAigerAsAIG = AIG.readAiger

-- | Read an AIGER file as a GIA network.
readAigerAsGIA :: FilePath -> IO (Network GIALit GIA)
readAigerAsGIA = GIA.readAiger
