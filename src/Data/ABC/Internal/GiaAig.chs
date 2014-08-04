{-# OPTIONS_GHC -fno-warn-unused-matches #-}

{- |
Module      : Data.ABC.Internal.GiaAig
Copyright   : Galois, Inc. 2010
License     : BSD3
Maintainer  : jhendrix@galois.com
Stability   : experimental
Portability : non-portable (c2hs, language extensions)

Comprehensive binding of @aig\/gia\/giaAig.h@ for
converting between AIGs (new-style, not ABC) and GIAs.

-}

module Data.ABC.Internal.GiaAig (
      Gia_AigMap_t
    , withGiaAigMap
    , aigDupGiaLit
    , giaManToAig
    ) where

import Control.Exception (bracket)
import Foreign
import Foreign.C

{#import Data.ABC.Internal.GIA #}
{#import Data.ABC.Internal.AIG #}

#include "giaAig.h"

{#fun Gia_ManToAig as ^
    { `Gia_Man_t'
    , id `CInt'
    } -> `Aig_Man_t' #}

type Gia_AigMap_t = Ptr Aig_Obj_t

-- | Create a new map for storing all nodes in GIA
newGiaAigMap :: Gia_Man_t -> IO Gia_AigMap_t
newGiaAigMap m = do
  n <- giaManObjNum m
  newArray (replicate (fromIntegral n) nullPtr)

withGiaAigMap :: Gia_Man_t -> (Gia_AigMap_t -> IO a) -> IO a
withGiaAigMap m = bracket (newGiaAigMap m) free

aigDupGiaLit :: Aig_Man_t -> Gia_AigMap_t -> Gia_Man_t -> GiaLit -> IO Aig_Obj_t
aigDupGiaLit aig m gia l = aigDupGiaLit' aig m gia (unGiaLit l)

foreign import ccall unsafe "AbcBridge_Aig_DupGiaLit"
  aigDupGiaLit' :: Ptr Aig_Man_t_ -> Gia_AigMap_t -> Gia_Man_t -> CInt -> IO Aig_Obj_t
