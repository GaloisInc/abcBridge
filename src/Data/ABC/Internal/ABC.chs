{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{- |
Module      : Data.ABC.Internal.ABC
Copyright   : Galois, Inc. 2010-2014
License     : BSD3
Maintainer  : jhendrix@galois.com
Stability   : experimental
Portability : non-portable (c2hs, language extensions)

/Incomplete./ Binding of @base\/base\/abc.h@ for manipulating and
running algorithms on the original ABC datatypes.

This current incomplete binding focuses on functions for manipulating
and-inverter graphs (AIGs).
-}

module Data.ABC.Internal.ABC
    (
    -- * Types
    -- ** Enums
      Abc_NtkType_t(..)
    , Abc_NtkFunc_t(..)
    , Abc_ObjType_t(..)
    -- ** Opague types
    , Abc_Ntk_t_
    , Abc_Obj_t_

    -- ** Pointer types
    , Abc_Ntk_t
    , Abc_Obj_t
    -- * Base
    -- ** Network getters
    , abcNtkFunc
    , abcNtkManName
    , abcNtkObjs
    , abcNtkPis
    , abcNtkPos
    , abcNtkCos
    , abcNtkCis
    , abcNtkObj
    , abcNtkManFunc
    , abcNtkModel
    , abcNtkExdc
    -- ** Counting objects
    , abcNtkPiNum
    , abcNtkPoNum
    , abcNtkCiNum
    , abcNtkCoNum
    , abcNtkLatchNum
    -- ** Creating simple objects
    , abcNtkCreateObj
    , abcObjNot
      -- ** Name manager
    , Nm_Man_t_
    , Nm_Man_t
    , nmManCreate
    , nmManFree

    -- ** Object getters
    , abcObjIsComplement
    , abcObjRegular
    , abcObjId
    , abcObjType
    , abcObjFanins
    , abcObjIsAnd
    , abcObjLit0
    , abcObjLit1
    , abcAigAnd
    , abcAigXor
    , abcAigMux
    , abcAigConst1
    , abcAigCleanup
    -- ** abcFanio.c
    -- | Functions for manipulating fanins and fanouts of a node.
    , abcObjAddFanin
    -- ** abcMiter.c
    -- | Functions for manipulating miters, a combination of two
    -- circuits that outputs @1@ if the outputs of the two original
    -- circuits would have been different.
    , abcNtkMiter
    , abcNtkMiterIsConstant
    -- ** abcNames.c
    -- | Functions for manipulating the names attached to distinguished
    -- nodes.  Many functions in ABC require that networks being
    -- combined be named equivalently, so adopting the canonical form
    -- by 'abcNtkShortNames' helps avoid name mismatch errors.
    , abcNtkShortNames
    -- ** abcNtk.c
    -- | Functions for allocating and deleting networks, each of which
    -- manages the memory of all nodes and other locations attached to it.
    , abcNtkAlloc
    , abcNtkDup
    , abcNtkDelete
    , p_abcNtkDelete
    -- ** abcObj.c
    -- | Functions for manipulating objects in networks.
    , abcNtkDeleteObj
    , abcNtkDeleteObjPo
    -- ** abcProve.c
    -- | Functions for performing SAT solving.
    , abcNtkIvyProve
    -- ** abcVerify.c
    -- | Functions for creating and testing counterexample models.
    , abcNtkVerifySimulatePattern
    -- ** abcBrdige_qbf
    , abcNtkQbf
    ) where

import Control.Monad
import Foreign
import Foreign.C

{#import Data.ABC.Internal.VecInt #}
{#import Data.ABC.Internal.VecPtr #}

import Data.ABC.Internal.Field

#include "abcbridge.h"
#include "abc.h"

data Nm_Man_t_

{#pointer *Nm_Man_t->Nm_Man_t_ #}

{#fun Nm_ManCreate as ^
   { id `CInt' -- nSize
   } -> `Nm_Man_t' #}

{#fun Nm_ManFree as ^
   { id `Nm_Man_t' -- p
   } -> `()' #}

cintEnum :: (Integral a, Enum b) => a -> b
cintEnum v = toEnum (fromIntegral v)

enumCInt :: Enum a => a -> CInt
enumCInt v = fromIntegral (fromEnum v)

{#enum Abc_NtkType_t {underscoreToCase} deriving (Show, Eq) #}
{#enum Abc_NtkFunc_t {underscoreToCase} deriving (Show, Eq) #}
{#enum Abc_ObjType_t {underscoreToCase} deriving (Show, Eq) #}
{#enum Abc_InitType_t {underscoreToCase} deriving (Show, Eq) #}

data Abc_Ntk_t_
data Abc_Obj_t_
data Abc_Aig_t_

-- | 'Abc_Obj_t' pointer that can have it's lower bit complemented,
-- and thus needs to be converted into an 'Abc_Obj_t' before you
-- can dereference it.

{#pointer *Abc_Ntk_t -> Abc_Ntk_t_ #}
{#pointer *Abc_Obj_t -> Abc_Obj_t_ #}
{#pointer *Abc_Aig_t -> Abc_Aig_t_ #}

-- | Default foreign pointer constructor that just calls free() on the
-- pointer when it is no longer being used.
-- XXX move this somewhere common
newDefaultForeignPtr :: Ptr a -> IO (ForeignPtr a)
newDefaultForeignPtr = newForeignPtr p_abcPrimFree

foreign import ccall unsafe "stdlib.h &free"
    p_abcPrimFree :: FunPtr (Ptr a -> IO ())

abcNtkCreateObj :: Abc_Ntk_t -> Abc_ObjType_t -> IO Abc_Obj_t
abcNtkCreateObj ntk tp = abcNtkCreateObj' ntk (enumCInt tp)

foreign import ccall unsafe "Abc_NtkCreateObj"
  abcNtkCreateObj' :: Abc_Ntk_t -> CInt -> IO Abc_Obj_t

-- inline definitions

abcNtkFunc :: Abc_Ntk_t -> IO Abc_NtkFunc_t
abcNtkFunc ntk = cintEnum `fmap` {#get Abc_Ntk_t->ntkFunc #} ntk

-- | Network name manager.
abcNtkManName :: Field Abc_Ntk_t Nm_Man_t
abcNtkManName = fieldFromOffset {#offsetof Abc_Ntk_t->pManName#}

-- | Return array of all objects.
abcNtkObjs :: Abc_Ntk_t -> IO Vec_Ptr_t
abcNtkObjs = {#get Abc_Ntk_t->vObjs #}

-- | Return primary inputs.
abcNtkPis :: Abc_Ntk_t -> IO Vec_Ptr_t
abcNtkPis = {#get Abc_Ntk_t->vPis #}

-- | Return primary outputs.
abcNtkPos :: Abc_Ntk_t -> IO Vec_Ptr_t
abcNtkPos = {#get Abc_Ntk_t->vPos #}

-- | Return combinational inputs (PIs, latches)
abcNtkCis :: Abc_Ntk_t -> IO Vec_Ptr_t
abcNtkCis = {#get Abc_Ntk_t->vCis #}

-- | Return combinational outputs (POs, asserts, latches).
abcNtkCos :: Abc_Ntk_t -> IO Vec_Ptr_t
abcNtkCos = {#get Abc_Ntk_t->vCos #}

-- | The functionality manager varies between 'AbcNtkFunc'.  In the case
-- of 'AbcFuncAig', this pointer is guaranteed to be an 'Abc_Aig_t'.
abcNtkManFunc :: Abc_Ntk_t -> IO (Ptr ())
abcNtkManFunc = {#get Abc_Ntk_t->pManFunc #}

-- | Return pointer to model associated with network.
abcNtkModel :: Abc_Ntk_t -> IO (Ptr CInt)
abcNtkModel = {#get Abc_Ntk_t->pModel #}

-- | The EXDC network.
abcNtkExdc :: Field Abc_Ntk_t Abc_Ntk_t
abcNtkExdc = fieldFromOffset {#offsetof Abc_Ntk_t->pExdc #}

abcNtkObj :: Abc_Ntk_t -> Int -> IO Abc_Obj_t
abcNtkObj ntk i = do
  v <- abcNtkObjs ntk
  vecPtrEntry v i

abcNtkPiNum :: Abc_Ntk_t -> IO Int
abcNtkPiNum = vecPtrSize <=< abcNtkPis

abcNtkPoNum :: Abc_Ntk_t -> IO Int
abcNtkPoNum = vecPtrSize <=< abcNtkPos

abcNtkCiNum :: Abc_Ntk_t -> IO Int
abcNtkCiNum = vecPtrSize <=< abcNtkCis

abcNtkCoNum :: Abc_Ntk_t -> IO Int
abcNtkCoNum = vecPtrSize <=< abcNtkCos

abcNtkObjCounts :: Abc_Ntk_t -> Ptr CInt
abcNtkObjCounts = (`plusPtr` {#offsetof Abc_Ntk_t->nObjCounts #})

abcNtkObjCount :: Abc_Ntk_t -> Abc_ObjType_t -> IO CInt
abcNtkObjCount p tp = peekElemOff (abcNtkObjCounts p) (fromEnum tp)

abcNtkLatchNum :: Abc_Ntk_t -> IO CInt
abcNtkLatchNum = (`abcNtkObjCount` AbcObjLatch)

-- | Object network
abcObjNtk :: Abc_Obj_t -> IO Abc_Ntk_t
abcObjNtk = {#get Abc_Obj_t->pNtk #}

-- | Object identifier.
abcObjId :: Abc_Obj_t -> IO CInt
abcObjId = {#get Abc_Obj_t->Id #}

-- | Object type.
abcObjType :: Abc_Obj_t -> IO Abc_ObjType_t
abcObjType o = cintEnum `fmap` {#get Abc_Obj_t->Type #} o

-- | Indicates if first fanin of object is complemented.
abcObjCompl0 :: Abc_Obj_t -> IO Bool
abcObjCompl0 o = cintEnum `fmap` {#get Abc_Obj_t->fCompl0 #} o

-- | Indicates if second fanin of object is complemented.
abcObjCompl1 :: Abc_Obj_t -> IO Bool
abcObjCompl1 o = cintEnum `fmap` {#get Abc_Obj_t->fCompl1 #} o

-- | Get object fanins.
abcObjFanins :: Abc_Obj_t -> Vec_Int_t
abcObjFanins = (`plusPtr` {#offsetof Abc_Obj_t->vFanins #})


abcObjFaninIdx :: Abc_Obj_t -> Int -> IO Int
abcObjFaninIdx o i = do
  fromIntegral `fmap` vecIntEntry (abcObjFanins o) (fromIntegral i)

-- | Return true if this an and gate.
abcObjIsAnd :: Abc_Obj_t -> IO Bool
abcObjIsAnd o = do
  fanin_count <- vecIntSize (abcObjFanins o)
  return (fanin_count == 2)

-- | Return fanin obj at given index.
abcFaninObj :: Abc_Ntk_t -> Abc_Obj_t -> Int -> IO Abc_Obj_t
abcFaninObj ntk o i = do
  objs <- abcNtkObjs ntk
  idx <- abcObjFaninIdx o i
  vecPtrEntry objs idx

abcObjNotIf :: Abc_Obj_t -> Bool -> Abc_Obj_t
abcObjNotIf o b = wordPtrToPtr $ ptrToWordPtr o `xor` (if b then 1 else 0)

abcObjLit0 :: Abc_Obj_t -> IO Abc_Obj_t
abcObjLit0 o = do
  ntk <- abcObjNtk o
  o0 <- abcFaninObj ntk o 0
  c0 <- abcObjCompl0 o
  return (o0 `abcObjNotIf` c0)

abcObjLit1 :: Abc_Obj_t -> IO Abc_Obj_t
abcObjLit1 o = do
  ntk <- abcObjNtk o
  o1 <- abcFaninObj ntk o 1
  c1 <- abcObjCompl1 o
  return (o1 `abcObjNotIf` c1)

-- | Return true if object is complemented.
abcObjIsComplement :: Abc_Obj_t -> Bool
abcObjIsComplement o = ptrToWordPtr o `testBit` 0

-- | Return  normalized object.
abcObjRegular :: Abc_Obj_t -> Abc_Obj_t
abcObjRegular o = wordPtrToPtr $ ptrToWordPtr o `clearBit` 0

-- | Negate object.
abcObjNot :: Abc_Obj_t -> Abc_Obj_t
abcObjNot o = wordPtrToPtr $ ptrToWordPtr o `xor` 1

-- abcAig.c

foreign import ccall unsafe "Abc_AigAnd"
  abcAigAnd :: Abc_Aig_t -> Abc_Obj_t -> Abc_Obj_t -> IO Abc_Obj_t

foreign import ccall unsafe "Abc_AigXor"
  abcAigXor :: Abc_Aig_t -> Abc_Obj_t -> Abc_Obj_t -> IO Abc_Obj_t

foreign import ccall unsafe "Abc_AigMux"
  abcAigMux :: Abc_Aig_t -> Abc_Obj_t -> Abc_Obj_t -> Abc_Obj_t -> IO Abc_Obj_t

{#fun Abc_AigConst1 as ^
    { id `Abc_Ntk_t'
    } -> `Abc_Obj_t' id #}

foreign import ccall unsafe "Abc_AigCleanup"
  abcAigCleanup :: Abc_Aig_t -> IO CInt

-- abcFanio.c

{#fun Abc_ObjAddFanin as ^
    { id `Abc_Obj_t' -- pObj
    , id `Abc_Obj_t' -- pFanin
    } -> `()' #}

-- abcMiter.c

{#fun Abc_NtkMiter as ^ -- XXX: assume that both Ntks are still usable
    { id `Abc_Ntk_t'
    , id `Abc_Ntk_t'
    , `Bool' -- fComb
    , `Int'  -- nPartSize
    , `Bool' -- fImplic
    , `Bool' -- fMulti
    } -> `Abc_Ntk_t' id #}

{#fun Abc_NtkMiterIsConstant as ^
    { id `Abc_Ntk_t'
    } -> `Int' #}

-- abcNames.c

{#fun Abc_NtkShortNames as ^
    { id `Abc_Ntk_t'
    } -> `()' #}

-- abcNtk.c

{#fun Abc_NtkAlloc as ^
    { enumCInt `Abc_NtkType_t'
    , enumCInt `Abc_NtkFunc_t'
    , `Bool' -- fUseMemMan
    } -> `Abc_Ntk_t' id #}

-- | Duplicate a network, allocating memory for the new network.  This
-- procedure does not preserve the @Id@ of objects.
{#fun Abc_NtkDup as ^
    { id `Abc_Ntk_t'
    } -> `Abc_Ntk_t' id #}

{#fun Abc_NtkDelete as ^
    { id `Abc_Ntk_t'
    } -> `()' #}


foreign import ccall unsafe "&Abc_NtkDelete"
    p_abcNtkDelete :: FunPtr (Abc_Ntk_t -> IO ())

-- abcObj.c

{#fun Abc_NtkDeleteObj as ^
    { id `Abc_Obj_t'
    } -> `()' #}

{#fun Abc_NtkDeleteObjPo as ^
    { id `Abc_Obj_t'
    } -> `()' #}

-- abcProve.c

{#fun Abc_NtkIvyProve as ^
    { id `Ptr Abc_Ntk_t'
    , id `Ptr ()'
    } -> `Int' #}

-- abcVerify.c

{#fun Abc_NtkVerifySimulatePattern as ^
    { id `Abc_Ntk_t'
    , id `Ptr CInt'
    } -> `ForeignPtr CInt' newDefaultForeignPtr* #}

-- abcBridge_qbv.c

{#fun AbcBridge_NtkQbf as abcNtkQbf
    { id `Abc_Ntk_t' -- Network
    , `Int' -- Number of parameters
    , id `CInt' -- Maximum number of iterations.
    , id `Vec_Int_t' -- Vector for storing result.
    } -> `Int' #}