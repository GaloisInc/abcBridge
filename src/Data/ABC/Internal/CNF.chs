{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{- |
Module      : Data.ABC.Internal.CNF
Copyright   : Galois, Inc. 2010-2014
License     : BSD3
Maintainer  : jhendrix@galois.com
Stability   : experimental
Portability : non-portable (c2hs, language extensions)
-}
module Data.ABC.Internal.CNF
  ( Cnf_Dat_t
  , cnfVarNums
  , withCnfDerive
  , cnfDataWriteIntoFile
  , Cnf_Man_t
  , Cnf_Man_t_
  , cnfManStart
  , cnfDeriveWithMan
  , cnfDataFree
  , cnfDataWriteIntoFileWithHeader
  ) where

import Control.Exception (bracket)
import Data.ABC.Internal.AIG
import Foreign
import Foreign.C

#include "cnfheader.h"

data Cnf_Man_t_

{#pointer *Aig_Man_t -> Aig_Man_t_ nocode #}

{#pointer *Cnf_Man_t foreign -> Cnf_Man_t_ #}

foreign import ccall unsafe "&Cnf_ManStop"
    p_cnfManStop :: FunPtr (Ptr Cnf_Man_t_ -> IO ())

newCnfMan :: Ptr Cnf_Man_t_ -> IO Cnf_Man_t
newCnfMan = newForeignPtr p_cnfManStop

-- | Create a new CNF manager.
{#fun Cnf_ManStart as ^
  {} -> `Cnf_Man_t' newCnfMan* #}

data Cnf_Dat_t_

{#pointer *Cnf_Dat_t -> Cnf_Dat_t_ #}

cnfVarNums :: Cnf_Dat_t -> IO (Ptr CInt)
cnfVarNums = {#get Cnf_Dat_t->pVarNums #}

{#fun Cnf_Derive as ^
      { id `Aig_Man_t'
      , id `CInt'
      }
      -> `Cnf_Dat_t' id #}

-- | Use results on cnfDerive in a comptuation, then free them.
withCnfDerive :: Aig_Man_t -> CInt -> (Cnf_Dat_t -> IO a) -> IO a
withCnfDerive mgr outputCount h =
  bracket (cnfDerive mgr outputCount) cnfDataFree h

{#fun Cnf_DeriveWithMan as ^ -- output
    { withForeignPtr* `Cnf_Man_t'
    , id `Aig_Man_t'
    , id `CInt'
    } -> `Cnf_Dat_t' id #}

{#fun Cnf_DataWriteIntoFile as ^
    { id `Cnf_Dat_t'
    , `String'
    , `Int'
    , id `Ptr ()'
    , id `Ptr ()'
    } -> `()' #}

{#fun Cnf_DataWriteIntoFileWithHeader as ^
    { id `Cnf_Dat_t'
    , `String' -- Filename
    , `String' -- Header
    , `Int'
    } -> `()' #}

{#fun Cnf_DataFree as ^
    { id `Cnf_Dat_t'
    } -> `()' #}
