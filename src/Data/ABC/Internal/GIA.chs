{- |
Module      : Data.ABC.Internal.GIA
Copyright   : Galois, Inc. 2010-2014
License     : BSD3
Maintainer  : jhendrix@galois.com
Stability   : experimental
Portability : non-portable (c2hs, language extensions)

Bindings to @aig\/gia\/gia.h@ for manipulating and
running algorithms on scalable and-inverter graphs (GIA), a
representation that is optimized for memory-efficiency.  These
functions power the next-generation of ABC algorithms that have
not been officially released yet, and can be identified by the
prefix of an ampersand, as in @&cec@, in the interactive ABC
interface.
-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Data.ABC.Internal.GIA (
      Gia_Man_t
    , Gia_Man_t_
    , giaManNObjs
    , giaManFanData
    , Gia_Obj_t
    , getGiaObjValue
    , setGiaObjValue
    , GiaVar(..)
    , GiaLit(..)
    , giaManConst0Lit
    , giaManConst1Lit
    , giaLitIsCompl
    , giaLitVar
    , giaVarLit
    , giaLitNotCond
    -- * Memory management
    -- * Base
    -- ** Network getters
    , giaManCexComb
    , giaManConst0
    , giaManCis
    , giaManCos
    , giaManCiNum
    , giaManCoNum
    , giaManPiNum
    , giaManPoNum
    , giaManAndNum
    , getGiaManRegNum
    , setGiaManRegNum

    , giaManCiVar
    , giaManCoVar
    , giaManCi
    , giaManCo
    , giaManObj
    , gia_none
    , giaObjIsCo
    , giaObjDiff0
    , giaObjDiff1
    , giaObjFaninC0
    , giaObjFaninC1
    , giaObjMark0
    , giaObjMark1
    , giaObjChild0
    , giaObjChild1
    , giaObjFaninId0
    , giaObjFaninId1
    , giaObjIsTerm
    , giaObjIsAndOrConst0
    , giaObjId
    , giaManObjNum
    -- ** Handling literals
    , giaLitNot
    , giaRegular
    , giaIsComplement
    , giaObjToLit
    , giaObjFromLit
    -- ** Iterators
    , giaManForEachObj1_
    , giaManForEachCo
    -- ** Construction
    , giaManAppendCi
    , giaManAppendCo
    , giaManAppendAnd
    -- * Functions
    -- ** giaAiger.c
    , giaAigerRead
    , giaAigerWrite
    -- ** giaDup.c
    , giaManMiter
    , giaDupLit
    , giaManDupNormalize
    -- ** giaHash.c
    , giaManHashAlloc
    , giaManHashStart
    , giaManHashStop
    , giaManHashAnd
    , giaManHashXor
    , giaManHashMux
     -- ** giaMan.c
    , giaManStart
    , giaManStop
    , p_giaManStop
    , giaManCleanup
    , giaManFillValue

      -- ** misc
    , clearGiaObj
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception
import Control.Monad
import Foreign hiding (void)
import Foreign.C

{#import Data.ABC.Internal.ABCGlobal #}
{#import Data.ABC.Internal.VecInt #}

import Data.ABC.Internal.Field

#include "abcbridge.h"
#include "gia.h"

-- Iterators

enumRange :: (Eq a, Enum a) => a -> a -> [a]
enumRange i n | i == n = []
              | otherwise = i : enumRange (succ i) n

lazyAnd :: Monad m => m Bool -> m Bool -> m Bool
lazyAnd mx my = do
  x <- mx
  if x then my else return False

asWordPtr :: (WordPtr -> WordPtr) -> Ptr a -> Ptr b
asWordPtr f = wordPtrToPtr . f . ptrToWordPtr

------------------------------------------------------------------------
-- Gia_Obj_t

data Gia_Obj_t_

-- | A pointer to a GIA object.
-- GIA objects are pointers to structs in ABC, and represent literals
-- in the AIG.  The low-order bit of the pointer is set to 1 if the
-- literal has been complemented, and so care must be taken to only
-- dereference positive pointers.  The object is also a bitfield, so
-- care must be taken when accessing fields.
--
-- Pointers to GIA objects may be invalidated when adding a new
-- object.
{#pointer *Gia_Obj_t -> Gia_Obj_t_ #}

sizeOfGiaObj :: Int
sizeOfGiaObj = {#sizeof Gia_Obj_t #}

-- | Remove negation.
giaRegular :: Gia_Obj_t -> Gia_Obj_t
giaRegular = asWordPtr (.&. complement 0x1)

-- | Returns iDiff0 field of object
-- Note: iDiff0 is a bitfield, so this may be more likely to break on
-- unexpected compilers.
giaObjDiff0 :: Gia_Obj_t -> IO CUInt
giaObjDiff0 = {#get Gia_Obj_t->iDiff0 #}

-- | Get the complement attribute of first fanin
giaObjFaninC0 :: Gia_Obj_t -> IO Bool
giaObjFaninC0 o = toBool `fmap` {#get Gia_Obj_t->fCompl0 #} o

-- | Get first user defined mark
giaObjMark0 :: Gia_Obj_t -> IO Bool
giaObjMark0 o = toBool `fmap` {#get Gia_Obj_t->fMark0 #} o

giaObjIsTerm :: Gia_Obj_t -> IO Bool
giaObjIsTerm o = toBool `fmap` {#get Gia_Obj_t->fTerm #} o

-- | Returns iDiff1 field of object
-- Note: iDiff1 is a bitfield, so this may be more likely to break on
-- unexpected compilers.
giaObjDiff1 :: Gia_Obj_t -> IO CUInt
giaObjDiff1 = {#get Gia_Obj_t->iDiff1 #}

giaObjFaninC1 :: Gia_Obj_t -> IO Bool
giaObjFaninC1 o = toBool `fmap` {#get Gia_Obj_t->fCompl1 #} o

-- | Get second user defined mark
giaObjMark1 :: Gia_Obj_t -> IO Bool
giaObjMark1 o = toBool `fmap` {#get Gia_Obj_t->fMark1 #} o

getGiaObjValue :: Gia_Obj_t -> IO CUInt
getGiaObjValue = {#get Gia_Obj_t->Value #}

setGiaObjValue :: Gia_Obj_t -> CUInt -> IO ()
setGiaObjValue = {#set Gia_Obj_t->Value #}

-- GIA_NONE
gia_none :: CUInt
gia_none = 0x1FFFFFFF

giaObjIsAndOrConst0 :: Gia_Obj_t -> IO Bool
giaObjIsAndOrConst0 o = not <$> giaObjIsTerm o

giaObjDiff0Assigned :: Gia_Obj_t -> IO Bool
giaObjDiff0Assigned o = (/= gia_none) <$> giaObjDiff0 o

giaIsComplement :: Gia_Obj_t -> Bool
giaIsComplement o = ptrToWordPtr o `testBit` 0

giaNot :: Gia_Obj_t -> Gia_Obj_t
giaNot = asWordPtr (xor 1)

giaNotCond :: Gia_Obj_t -> Bool -> Gia_Obj_t
giaNotCond o b = if b then giaNot o else o

-- | Returns true if this is a combinational output (latch or primary output).
giaObjIsCo :: Gia_Obj_t -> IO Bool
giaObjIsCo o = lazyAnd (giaObjIsTerm o) (giaObjDiff0Assigned o)

incObjPtr :: Gia_Obj_t -> CInt -> Gia_Obj_t
incObjPtr o i = o `plusPtr` (sizeOfGiaObj * fromIntegral i)

decObjPtr :: Gia_Obj_t -> CUInt -> Gia_Obj_t
decObjPtr o i = incObjPtr o (negate (fromIntegral i))

objDiff :: Gia_Obj_t -> Gia_Obj_t -> Int
objDiff p q = (p `minusPtr` q) `div` sizeOfGiaObj

-- Gia_ObjFanin0
giaObjFanin0 :: Gia_Obj_t -> IO Gia_Obj_t
giaObjFanin0 o = decObjPtr o <$> giaObjDiff0 o

-- Gia_ObjFanin1
giaObjFanin1 :: Gia_Obj_t -> IO Gia_Obj_t
giaObjFanin1 o = decObjPtr o <$> giaObjDiff1 o

-- Gia_ObjChild0
giaObjChild0 :: Gia_Obj_t -> IO Gia_Obj_t
giaObjChild0 o = giaNotCond <$> giaObjFanin0 o <*> giaObjFaninC0 o

-- Gia_ObjChild1
giaObjChild1 :: Gia_Obj_t -> IO Gia_Obj_t
giaObjChild1 o = giaNotCond <$> giaObjFanin1 o <*> giaObjFaninC1 o

-- Gia_ObjFaninId0
giaObjFaninId0 :: Gia_Obj_t -> GiaVar -> IO GiaVar
giaObjFaninId0 o (GiaVar v) = (\d -> GiaVar (v - fromIntegral d)) <$> giaObjDiff0 o

-- Gia_ObjFaninId1
giaObjFaninId1 :: Gia_Obj_t -> GiaVar -> IO GiaVar
giaObjFaninId1 o (GiaVar v) = (\d -> GiaVar (v - fromIntegral d)) <$> giaObjDiff1 o

------------------------------------------------------------------------
-- GiaVar and GiaLit

-- | Also known as the node's id.  No complement info.
newtype GiaVar = GiaVar { unGiaVar :: CInt } deriving (Eq, Ord, Storable)

-- | Literals store complement information.
newtype GiaLit = GiaLit { unGiaLit :: CInt } deriving (Eq, Ord, Storable)

giaManConst0Lit :: GiaLit
giaManConst0Lit = GiaLit 0

giaManConst1Lit :: GiaLit
giaManConst1Lit = GiaLit 1

giaLitIsCompl :: GiaLit -> Bool
giaLitIsCompl l = unGiaLit l `testBit` 0

giaLitVar :: GiaLit -> GiaVar
giaLitVar (GiaLit l) = GiaVar (l `shiftR` 1)

-- | Returns positive literal associated to var.
giaVarLit :: GiaVar -> GiaLit
giaVarLit (GiaVar v) = GiaLit (v `shiftL` 1)

giaLitNot :: GiaLit -> GiaLit
giaLitNot = GiaLit . xor 1 . unGiaLit

giaLitNotCond :: GiaLit -> Bool -> GiaLit
giaLitNotCond (GiaLit l) b = GiaLit (l `xor` (if b then 1 else 0))

------------------------------------------------------------------------
-- Gia_Man_t

data Gia_Man_t_
{#pointer *Gia_Man_t -> Gia_Man_t_ #}

-- member access

-- this structures are memory managed by Gia_Man_.
giaManCexComb :: Gia_Man_t -> IO Abc_Cex_t
giaManCexComb = {#get Gia_Man_t->pCexComb #}

giaManNObjs :: Field Gia_Man_t CInt
giaManNObjs = fieldFromOffset {#offsetof Gia_Man_t->nObjs #}

giaManObjs :: Field Gia_Man_t Gia_Obj_t
giaManObjs = fieldFromOffset {#offsetof Gia_Man_t->pObjs #}

giaManConst0 :: Gia_Man_t -> IO Gia_Obj_t
giaManConst0 = readAt giaManObjs

giaManCis :: Gia_Man_t -> IO Vec_Int_t
giaManCis = {#get Gia_Man_t->vCis #}

giaManCos :: Gia_Man_t -> IO Vec_Int_t
giaManCos = {#get Gia_Man_t->vCos #}

giaManFanData :: Gia_Man_t -> IO (Ptr CInt)
giaManFanData = {#get Gia_Man_t->pFanData#}

giaManObjNum :: Gia_Man_t -> IO CInt
giaManObjNum = readAt giaManNObjs

giaManCiNum :: Gia_Man_t -> IO CInt
giaManCiNum = vecIntSize <=< giaManCis

giaManCoNum :: Gia_Man_t -> IO CInt
giaManCoNum = vecIntSize <=< giaManCos

getGiaManRegNum :: Gia_Man_t -> IO CInt
getGiaManRegNum = {#get Gia_Man_t->nRegs #}

setGiaManRegNum :: Gia_Man_t -> CInt -> IO ()
setGiaManRegNum = {#set Gia_Man_t->nRegs #}

giaManPiNum :: Gia_Man_t -> IO CInt
giaManPiNum m = (-) <$> giaManCiNum m <*> getGiaManRegNum m

giaManPoNum :: Gia_Man_t -> IO CInt
giaManPoNum m = (-) <$> giaManCoNum m <*> getGiaManRegNum m

giaManAndNum :: Gia_Man_t -> IO CInt
giaManAndNum m = fn <$> giaManObjNum m <*> giaManCiNum m <*> giaManCoNum m
  where fn t i o = t - i - o - 1

-- XXX refactor
giaManForEachObj1_ :: Gia_Man_t -> (Gia_Obj_t -> GiaVar -> IO b) -> IO ()
giaManForEachObj1_ fp action = do
  nMax <- giaManObjNum fp
  forM_ (enumRange 1 nMax) $ \i -> do
    let var = GiaVar (fromIntegral i)
    pObj <- giaManObj fp var
    void $ action pObj var

giaManForEachCo :: Gia_Man_t -> (Gia_Obj_t -> Int -> IO b) -> IO [b]
giaManForEachCo fp action = do
  nMax <- giaManCoNum fp
  forM (enumRange 0 nMax) $ \i -> do
    pObj <- giaManCo fp i
    action pObj (fromIntegral i)

foreign import ccall unsafe "AbcBridge_Gia_ManAppendCi"
  giaManAppendCi_ :: Gia_Man_t -> IO CInt

giaManAppendCi :: Gia_Man_t -> IO GiaLit
giaManAppendCi m = GiaLit <$> giaManAppendCi_ m

foreign import ccall unsafe "AbcBridge_Gia_ManAppendAnd"
  giaManAppendAnd_ :: Gia_Man_t -> CInt -> CInt -> IO CInt

-- | This directly appends the literal to the GIA bypassing
-- any hash-consing.
giaManAppendAnd :: Gia_Man_t -> GiaLit -> GiaLit -> IO GiaLit
giaManAppendAnd m (GiaLit x) (GiaLit y) =
  GiaLit <$> giaManAppendAnd_ m x y

foreign import ccall unsafe "AbcBridge_Gia_ManAppendCo"
  giaManAppendCo_ :: Gia_Man_t -> CInt -> IO CInt

giaManAppendCo :: Gia_Man_t -> GiaLit -> IO GiaLit
giaManAppendCo m (GiaLit l) = GiaLit <$> giaManAppendCo_ m l

-- | Return object associated with gia var.
giaManObj :: Gia_Man_t -> GiaVar -> IO Gia_Obj_t
giaManObj m (GiaVar v) = do
  cnt <- giaManObjNum m
  assert (0 <= v && v < cnt) $ do
    (`incObjPtr` v) <$> giaManConst0 m

-- | Get var index of combinational input at given index.
giaManCiVar :: Gia_Man_t -> CInt -> IO GiaVar
giaManCiVar m i = do
  v <- giaManCis m
  GiaVar <$> vecIntEntry v i

-- | Get combinational input at given index.
giaManCi :: Gia_Man_t -> CInt -> IO Gia_Obj_t
giaManCi m i = giaManObj m =<< giaManCiVar m i

-- | Get combinational output at given index.
giaManCoVar :: Gia_Man_t -> CInt -> IO GiaVar
giaManCoVar m i = do
  v <- giaManCos m
  GiaVar <$> vecIntEntry v i

-- | Get combinational output at given index.
giaManCo :: Gia_Man_t -> CInt -> IO Gia_Obj_t
giaManCo m i = giaManObj m =<< giaManCoVar m i

-- | Returns the variable index associated with the object.
giaObjId :: Gia_Man_t -> Gia_Obj_t -> IO GiaVar
giaObjId p pObj = do
  objs <- giaManConst0 p
  nObjs <- readAt giaManNObjs p
  assert (objs <= pObj && pObj < objs `incObjPtr` nObjs) $ do
    return $ GiaVar $ fromIntegral $ pObj `objDiff` objs

{#fun AbcBridge_Gia_ObjToLit as giaObjToLit
    { id `Gia_Man_t'
    , id `Gia_Obj_t'
    } -> `GiaLit' GiaLit #}
{#fun AbcBridge_Gia_ObjFromLit as giaObjFromLit
    { id `Gia_Man_t'
    , unGiaLit `GiaLit'
    } -> `Gia_Obj_t' id #}

-- giaAiger.c
{#fun Gia_AigerRead as ^
    { `String' -- pFileName
    , `Bool' -- fGiaSimple
    , `Bool' -- fSkipStrash
    , `Bool' -- fCheck (doesn't do anything)
    } -> `Gia_Man_t' id #}



{#fun Gia_AigerWrite as giaAigerWrite
    { id `Gia_Man_t'
    , `String' -- pFileName
    , `Bool' -- fWriteSymbols
    , `Bool' -- fCompact
    } -> `()' #}

{#fun Gia_ManMiter as giaManMiter
    { id `Gia_Man_t' -- pAig0cd
    , id `Gia_Man_t' -- pAig1
    , `Int'  -- nInsDup
    , `Bool' -- fDualOut
    , `Bool' -- fSeq
    , `Bool' -- fImplic
    , `Bool' -- fVerbose
    } -> `Gia_Man_t' id #}

foreign import ccall unsafe "Gia_ManDupNormalize" giaManDupNormalize
  :: Gia_Man_t -> IO Gia_Man_t

-- | @giaManDupDfsLazyLit pNew p l@ copies a lit @l@ in @p@ to @pNew@
-- and returns the lit in @pNew@.
giaDupLit :: Gia_Man_t -> Gia_Man_t -> GiaLit -> IO GiaLit
giaDupLit pNew p (GiaLit l) = GiaLit <$> giaDupLit' pNew p l

foreign import ccall unsafe "AbcBridge_Gia_DupLit" giaDupLit'
  :: Gia_Man_t -> Gia_Man_t -> CInt -> IO CInt

-- giaHash.c
foreign import ccall unsafe "Gia_ManHashAlloc" giaManHashAlloc
  :: Gia_Man_t -> IO ()

foreign import ccall unsafe "Gia_ManHashStart" giaManHashStart
  :: Gia_Man_t -> IO ()

foreign import ccall unsafe "Gia_ManHashStop" giaManHashStop
  :: Gia_Man_t -> IO ()

{#fun Gia_ManHashAnd as ^
    { id `Gia_Man_t' -- p
    , unGiaLit `GiaLit' -- iLit0
    , unGiaLit `GiaLit' -- iLit1
    } -> `GiaLit' GiaLit #}
{#fun Gia_ManHashXor as ^
    { id `Gia_Man_t' -- p
    , unGiaLit `GiaLit' -- iLit0
    , unGiaLit `GiaLit' -- iLit1
    } -> `GiaLit' GiaLit #}
{#fun Gia_ManHashMux as ^
    { id `Gia_Man_t' -- p
    , unGiaLit `GiaLit' -- iCtrl
    , unGiaLit `GiaLit' -- iData1
    , unGiaLit `GiaLit' -- iData0
    } -> `GiaLit' GiaLit #}

-- giaMan.c
foreign import ccall unsafe "Gia_ManStop"
    giaManStop :: Gia_Man_t -> IO ()

foreign import ccall unsafe "gia.h &Gia_ManStop"
    p_giaManStop :: FunPtr (Gia_Man_t -> IO ())

foreign import ccall unsafe "AbcBridge_Gia_ClearGiaObj"
    clearGiaObj :: Gia_Obj_t -> IO ()

{#fun Gia_ManStart as ^ { id `CInt' } -> `Gia_Man_t' id #}

{#fun Gia_ManCleanup as ^
    { id `Gia_Man_t' -- p
    } -> `Gia_Man_t' id #}

{#fun Gia_ManFillValue as ^
    { id `Gia_Man_t' -- p
    } -> `()' #}
