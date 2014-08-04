{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

{- |
Module      : Data.ABC.Internal.IO
Copyright   : Galois, Inc. 2010-2014
License     : BSD3
Maintainer  : jhendrix@galois.com
Stability   : experimental
Portability : non-portable (c2hs, language extensions)

Binding of @base\/io\/io.h@ for reading and writing
networks to the file system.  ABC natively supports a variety of
different file formats.
-}

module Data.ABC.Internal.IO (
    -- * abcReadAiger.c
      ioReadAiger
    -- * abcWriteAiger.c
    , ioWriteAiger
    ) where

import Foreign
import Foreign.C

{#import Data.ABC.Internal.ABC #}

#include "ioAbc.h"

-- abcReadAiger.c
{#fun Io_ReadAiger as ^ {`String', `Bool'} -> `Abc_Ntk_t' id #}
-- abcWriteAiger.c
{#fun Io_WriteAiger as ^
    { id `Abc_Ntk_t'
    , `String'
    , `Bool' -- fWriteSymbols
    , `Bool' -- fCompact
    , `Bool' -- fUnique
    } -> `()' #}
