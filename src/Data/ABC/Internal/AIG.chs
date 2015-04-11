{- |
Module      : Data.ABC.Internal.AIG
Copyright   : Galois, Inc. 2010
License     : BSD3
Maintainer  : jhendrix@galois.com
Stability   : experimental
Portability : non-portable (c2hs, language extensions)

/Incomplete./ Binding of @aig\/aig\/aig.h@.  This defines the
next-generation heavy-weight AIG representation (similar to the original
"Data.ABC.Internal.ABC") which is used in internal versions
(@base\/abci\/abc.c@) 8D, 8 and occasionally for 9 (during which the GIA
is temporarily converted into an AIG for some processing.)
-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Data.ABC.Internal.AIG (
    -- * Types
    -- ** Enums
      Aig_Type_t(..)
    -- ** Empty types
    , Aig_Man_t_
    , Aig_Obj_t_
    -- ** Pointer types
    , Aig_Man_t
    , Aig_Obj_t
    -- * Memory management
    , aigRegular
    , aigNot
    , aigNotCond
    , aigIsComplement
    , aigManCiNum
    , aigManCoNum
    , aigManObjNumMax
    , aigManConst0
    , aigManConst1
    , aigManCi
    , aigManCo
    , aigObjId
      -- * aigMan.c
    , aigManStart
    , aigManStop
    , p_aigManStop
      -- * aigObj.c
    , aigObjCreateCi
    , aigObjCreateCo
      -- * Re-exports
    , CInt
    ) where

#include "aig.h"
#include "abcbridge.h"

import Foreign
import Foreign.C

{#import Data.ABC.Internal.VecPtr #}

{#enum Aig_Type_t {underscoreToCase} deriving (Show, Eq) #}

data Aig_Man_t_
data Aig_Obj_t_

{#pointer *Aig_Man_t -> Aig_Man_t_ #}
{#pointer *Aig_Obj_t -> Aig_Obj_t_ #}

aigObjWordPtr :: (WordPtr -> WordPtr) -> (Aig_Obj_t -> Aig_Obj_t)
aigObjWordPtr f = wordPtrToPtr . f . ptrToWordPtr

aigRegular :: Aig_Obj_t -> Aig_Obj_t
aigRegular = aigObjWordPtr (`clearBit` 0)

aigNot :: Aig_Obj_t -> Aig_Obj_t
aigNot = aigObjWordPtr (xor (bit 0))

aigNotCond :: Aig_Obj_t -> Bool -> Aig_Obj_t
aigNotCond o b = if b then aigNot o else o

aigIsComplement :: Aig_Obj_t -> Bool
aigIsComplement o = ptrToWordPtr o `testBit` 0

typeCInt :: Aig_Type_t -> CInt
typeCInt = fromIntegral . fromEnum

{#fun AbcBridge_Aig_ManNObj as aigManNObj
    { id `Aig_Man_t'
    , typeCInt `Aig_Type_t'
    } -> `CInt' id #}

aigManCiNum :: Aig_Man_t -> IO CInt
aigManCiNum = flip aigManNObj AigObjCi

aigManCoNum :: Aig_Man_t -> IO CInt
aigManCoNum = flip aigManNObj AigObjCo

aigManObjNumMax :: Aig_Man_t -> IO Int
aigManObjNumMax man =
  vecPtrSize =<< {#get Aig_Man_t->vObjs #} man

aigManConst0 :: Aig_Man_t -> IO Aig_Obj_t
aigManConst0 m = aigNot `fmap` aigManConst1 m

aigManConst1 :: Aig_Man_t -> IO Aig_Obj_t
aigManConst1 = {#get Aig_Man_t->pConst1 #}

{#fun AbcBridge_Aig_ManCi as aigManCi
    { id `Aig_Man_t'
    , id `CInt'
    } -> `Aig_Obj_t' id #}
{#fun AbcBridge_Aig_ManCo as aigManCo
    { id `Aig_Man_t'
    , id `CInt'
    } -> `Aig_Obj_t' id #}

-- Note: We use a function rather rather than just #get, because Aig_Obj_t
-- is a bitfield, and offsets are computed incorrectly by c2hs.
{#fun AbcBridge_Aig_ObjId as aigObjId
    { id `Aig_Obj_t' } -> `CInt' id #}

{-
newAigManForeignPtr :: Ptr Aig_Man_t_ -> IO (Aig_Man_t)
newAigManForeignPtr ptr | ptr == nullPtr = throwIO NullPtrError
                        | otherwise      = newForeignPtr p_aigManStop ptr
-}

foreign import ccall unsafe "&Aig_ManStop"
    p_aigManStop :: FunPtr (Aig_Man_t -> IO ())

foreign import ccall unsafe "Aig_ManStop" aigManStop :: Aig_Man_t -> IO ()

{#fun Aig_ManStart as ^ { id `CInt' } -> `Aig_Man_t' id #}

{#fun Aig_ObjCreateCi as ^
   { id `Aig_Man_t' } -> `Aig_Obj_t' id #}
{#fun Aig_ObjCreateCo as ^
   { id `Aig_Man_t', id `Aig_Obj_t' } -> `Aig_Obj_t' id  #}
