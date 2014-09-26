{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Data.ABC.AIG
Copyright   : Galois, Inc. 2010-2014
License     : BSD3
Maintainer  : jhendrix@galois.com
Stability   : stable
Portability : non-portable (language extensions)

"Data.ABC.AIG" defines a set of higher level functions for manipulating
and-inverter graph networks ('AIG') directly from ABC.  This module
should be imported @qualified@, e.g.

> import Data.ABC.AIG (AIG)
> import qualified Data.ABC.AIG as AIG

-}

module Data.ABC.AIG
  ( AIG
  , newAIG
  , readAiger
  , proxy
  , Lit
  , true
  , false
  , writeToCNF
  , writeAIGManToCNFWithMapping
  , checkSat'
    -- * Re-exports
  , AIG.Network(..)
  , AIG.networkInputCount
  , AIG.IsAIG(..)
  , AIG.IsLit(..)
  , AIG.SatResult(..)
  , AIG.VerifyResult(..)
  , AIG.SomeGraph(..)
  ) where

import Prelude hiding (and, or, not)

import Foreign

import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import System.IO
import qualified System.IO.Unsafe as Unsafe
import qualified Data.Map as Map
import           Data.IORef



import Data.ABC.Internal.ABC
import Data.ABC.Internal.AIG
import Data.ABC.Internal.CNF
import Data.ABC.Internal.Field
import Data.ABC.Internal.FRAIG
import Data.ABC.Internal.IO
import Data.ABC.Internal.Main
import Data.ABC.Internal.Orphan
import Data.ABC.Internal.VecInt
import Data.ABC.Internal.VecPtr

import qualified Data.AIG as AIG
import           Data.AIG.Interface (LitView(..))
import qualified Data.AIG.Trace as Tr
import Data.ABC.Util

newtype AIG s = AIG { _ntkPtr :: ForeignPtr Abc_Ntk_t_ }

newtype Lit s = Lit { unLit :: Abc_Obj_t }
  deriving (Eq, Storable, Ord)

-- | Proxy for building AIG networks
proxy :: AIG.Proxy Lit AIG
proxy = AIG.Proxy id

-- | Build a new, empty AIG graph
newAIG :: IO (AIG.SomeGraph AIG)
newAIG = do
  abcStart -- Ensure's ABC has been initialized.
  bracketOnError (abcNtkAlloc AbcNtkStrash AbcFuncAig True) abcNtkDelete $ \p -> do
    AIG.SomeGraph . AIG <$> newForeignPtr p_abcNtkDelete p

foreachPo :: Abc_Ntk_t -> (Abc_Obj_t -> IO a) -> IO [a]
foreachPo ntk f = do
  v <- abcNtkPos ntk
  c <- vecPtrSize v
  forN c $ \i -> do
    f =<< vecPtrEntry v i

foreachPo_ :: Abc_Ntk_t -> (Abc_Obj_t -> IO ()) -> IO ()
foreachPo_ ntk f = do
  v <- abcNtkPos ntk
  c <- vecPtrSize v
  forN_ c (f <=< vecPtrEntry v)


-- | Delete all primary outputs.
deletePos :: Abc_Ntk_t -> IO ()
deletePos p = do
   foreachPo_ p abcNtkDeleteObjPo
   clearVecPtr =<< abcNtkPos p
   clearVecPtr =<< abcNtkCos p

-- | Check that a file can be read.
-- N.B. We should eventually modify abc to be safe.
checkReadable :: FilePath -> IO ()
checkReadable path = withFile path ReadMode (\_ -> return())

-- | Read an AIGER file as an AIG network
readAiger :: FilePath -> IO (AIG.Network Lit AIG)
readAiger path = do
  abcStart
  checkReadable path
  bracketOnError (ioReadAiger path True) abcNtkDelete $ \p -> do
    lc <- abcNtkLatchNum p
    when (lc > 0) $ fail "Reading networks with latches is not yet supported."
    -- Clear name man table.
    nmManFree =<< readAt abcNtkManName p
    writeAt abcNtkManName p =<< nmManCreate 1
    -- Delete all primary outputs.
    outputs <- foreachPo p $ \o -> do
      i <- vecIntEntry (abcObjFanins o) 0
      abcNtkDeleteObjPo o
      Lit <$> abcNtkObj p (fromIntegral i)

    -- clear the PO and CO vector
    clearVecPtr =<< abcNtkCos p
    clearVecPtr =<< abcNtkPos p

    -- Return new pointer.
    fp <- newForeignPtr p_abcNtkDelete p
    return (AIG.Network (AIG fp) outputs)

withAIGPtr :: AIG s -> (Abc_Ntk_t -> IO a) -> IO a
withAIGPtr (AIG fp) m = withForeignPtr fp m

instance AIG.IsLit Lit where
  not (Lit l) = Lit (abcObjNot l)
  Lit x === Lit y = x == y

{-# NOINLINE true #-}
true :: AIG s -> Lit s
true a = Unsafe.unsafePerformIO $ do
  Lit <$> withAIGPtr a abcAigConst1

false :: AIG s -> Lit s
false a = AIG.not (true a)

-- | Returns true is the literal is satisfiabile.
checkSat' :: Ptr Abc_Ntk_t -> IO AIG.SatResult
checkSat' pp = do
  p <- peek pp
  ic <- vecPtrSize =<< abcNtkPis p
  oc <- vecPtrSize =<< abcNtkPos p
  assert (oc == 1) $ do
  isConstant <- checkIsConstant p
  case isConstant of
    Just True -> return (AIG.Sat (replicate ic False))
    Just False  -> return AIG.Unsat
    Nothing -> do
      let params = proveParamsDefault { nItersMax'Prove_Params = 5 }
      with params $ \pParams -> do
        r <- abcNtkIvyProve pp (castPtr pParams)
        case r of
          -1 -> fail "Could not decide equivalence."
          0 -> do
            p1 <- peek pp
            pModel <- abcNtkModel p1
            AIG.Sat . fmap toBool <$> peekArray ic pModel
          1 -> return AIG.Unsat
          _ -> error $ "Unrecognized return code " ++ show r ++ " from abcNtkIvyProve"

memoFoldAIG :: AIG s -> (LitView a -> IO a) -> IO (Lit s -> IO a)
memoFoldAIG g view = do
    r <- newIORef Map.empty

    let memo o t = do
           m <- readIORef r
           writeIORef r $! Map.insert o t m
           return t

        go (And x y)    = view =<< (pure And <*> objTerm x <*> objTerm y)
        go (NotAnd x y) = view =<< (pure NotAnd <*> objTerm x <*> objTerm y)
        go (Input i)    = view (Input i)
        go (NotInput i) = view (NotInput i)
        go TrueLit      = view TrueLit
        go FalseLit     = view FalseLit

        objTerm o = do
           m <- readIORef r
           case Map.lookup o m of
              Just t -> return t
              _ -> memo o =<< go =<< litView o

    -- NB: Pin down the AIG foreign pointer, even though we don't explicitly use it
    return $ (\l -> withAIGPtr g $ \_p -> objTerm l)

-- Return a representation of how lit was constructed.
-- NB: hold the AIG pointer to the graph to call this function...
litView :: Lit s -> IO (LitView (Lit s))
litView (Lit l) = do
  let c = abcObjIsComplement l
  let o = abcObjRegular l
  i <- abcObjId o
  ty <- abcObjType o
  case ty of
    AbcObjPi -> if c then return (NotInput (fromIntegral (i-1))) else return (Input (fromIntegral (i-1)))
    AbcObjConst1 -> if c then return FalseLit else return TrueLit
    AbcObjNode -> do
     isand <- abcObjIsAnd o
     if isand
       then do
         x <- abcObjLit0 o
         y <- abcObjLit1 o
         if c then return (NotAnd (Lit x) (Lit y))
              else return (And (Lit x) (Lit y))
       else fail "invalid AIG literal: non-and node"
    _ -> fail ("invalid AIG literal: "++show ty++" "++show i++" "++show c)

instance Tr.Traceable Lit where
  compareLit x y = compare x y
  showLit x = show (unLit x)


instance AIG.IsAIG Lit AIG where
  newGraph _ = newAIG
  aigerNetwork _ = readAiger

  trueLit = true
  falseLit = false

  newInput a =
    withAIGPtr a $ \p -> do
      Lit <$> abcNtkCreateObj p AbcObjPi
  and a x y = do
    withAIGPtr a $ \p -> do
     manFunc <- castPtr <$> abcNtkManFunc p
     Lit <$> abcAigAnd manFunc (unLit x) (unLit y)
  xor a x y = do
    withAIGPtr a $ \p -> do
      manFunc <- castPtr <$> abcNtkManFunc p
      Lit <$> abcAigXor manFunc (unLit x) (unLit y)
  mux a c t f = do
    withAIGPtr a $ \p -> do
      manFunc <- castPtr <$> abcNtkManFunc p
      Lit <$> abcAigMux manFunc (unLit c) (unLit t) (unLit f)

  inputCount a = withAIGPtr a (vecPtrSize <=< abcNtkPis)

  getInput a i = do
    withAIGPtr a $ \p -> do
      v <- abcNtkPis p
      sz <- vecPtrSize v
      assert (0 <= i && i < sz) $
        Lit . castPtr <$> vecPtrEntry v i

  writeAiger path a = do
    withNetworkPtr a $ \p -> do
      ioWriteAiger p path True False False

  checkSat g l = do
    withNetworkPtr (AIG.Network g [l]) $ \p -> do
      alloca $ \pp -> do
        poke pp =<< abcNtkDup p
        flip finally (abcNtkDelete =<< peek pp) $ do
        checkSat' pp

  abstractEvaluateAIG = memoFoldAIG

  cec x y = do
    ix <- networkInputCount x
    iy <- networkInputCount y
    assert (ix == iy) $ do
    assert (outputCount x == outputCount y) $ do
    withNetworkPtr x $ \xp -> do
    withNetworkPtr y $ \yp -> do
      alloca $ \pp -> do
        flip finally (abcNtkDelete =<< peek pp) $ do
          poke pp =<< abcNtkMiter xp yp False 0 False False
          AIG.toVerifyResult <$> checkSat' pp

  evaluator g inputs_l = do
    withAIGPtr g $ \ntk -> do
      -- Get vector with objects.
      objs <- abcNtkObjs ntk
      -- Get number of objects
      var_count <- vecPtrSize objs

      v <- VM.new var_count

      -- Initialize constant literal value.
      VM.write v 0 True

      -- Initialize primary input.
      pis <- abcNtkPis ntk
      pi_count <- vecPtrSize pis
      let inputs = V.fromList inputs_l
      when (V.length inputs /= pi_count) $
        fail "evaluate given unexpected number of inputs."

      forI_ pi_count $ \pi_idx -> do
        o <- vecPtrEntry pis pi_idx
        idx <- fromIntegral <$> abcObjId o
        VM.write v idx (inputs V.! pi_idx)

      -- Initialize and nodes.
      forI_ var_count $ \i -> do
        o <- vecPtrEntry objs i
        -- skip deleted vars!
        unless (o == nullPtr) $ do
        is_and <- abcObjIsAnd o
        when is_and $ do
          r0 <- evaluateFn v . Lit =<< abcObjLit0 o
          r1 <- evaluateFn v . Lit =<< abcObjLit1 o
          VM.write v i (r0 && r1)
      -- Return evaluation function.
      pureEvaluateFn g <$> V.freeze v

forI_ :: Monad m => Int -> (Int -> m ()) -> m ()
forI_ = go 0
  where go i n f | i < n = f i >> go (i+1) n f
                 | otherwise = return ()

{-# NOINLINE pureEvaluateFn #-}
pureEvaluateFn :: AIG s -> V.Vector Bool -> Lit s -> Bool
pureEvaluateFn g v (Lit l) = Unsafe.unsafePerformIO $ withAIGPtr g $ \_ -> do
  let c = abcObjIsComplement l
  let o = abcObjRegular l
  i <- fromIntegral <$> abcObjId o
  let n = V.length v
  when (i >= n) $ fail "Literal created after evaluator was created."
  return ((v V.! i) /= c)

evaluateFn :: VM.IOVector Bool
           -> Lit s
           -> IO Bool
evaluateFn v (Lit l) = do
  let c = abcObjIsComplement l
  let o = abcObjRegular l
  i <- fromIntegral <$> abcObjId o
  let n = VM.length v
  when (i >= n) $ fail "Literal created after evaluator was created."

  r <- VM.read v i
  return (r /= c)

networkInputCount :: AIG.Network l g -> IO Int
networkInputCount (AIG.Network g _) = AIG.inputCount g

outputCount :: AIG.Network l g -> Int
outputCount (AIG.Network _ o) = length o

withNetworkPtr :: AIG.Network Lit AIG
               -> (Abc_Ntk_t -> IO a)
               -> IO a
withNetworkPtr (AIG.Network x o) m = do
  withAIGPtr x $ \p -> do
  flip finally (deletePos p) $ do
    mapM_ (addPo p) o
    m p

addPo :: Abc_Ntk_t -> Lit s -> IO ()
addPo p (Lit ptr) = do
  po <- abcNtkCreateObj p AbcObjPo
  abcObjAddFanin po ptr



checkIsConstant :: Abc_Ntk_t -> IO (Maybe Bool)
checkIsConstant p = do
  c <- abcNtkMiterIsConstant p
  case c of
    -1 -> return Nothing
    0 -> return (Just True)
    1 -> return (Just False)
    _ -> error $ "Unrecognized return code " ++ show c ++ " from abcNtkMiterIsConstant"

-- | Run a computation using a handle to a new AIG Manager, and then free
-- the AIG manager.
withAbcNtkToDar :: Abc_Ntk_t
                -> Bool
                -> Bool
                -> (Aig_Man_t -> IO a)
                -> IO a
withAbcNtkToDar ntk exors registers h = do
  bracket (abcNtkToDar ntk exors registers)
          aigManStop
          h

-- | Write a CNF file to the given path.
-- Returns vector mapping combinational inputs to CNF Variable numbers.
writeToCNF :: AIG s -> Lit s -> FilePath -> IO [Int]
writeToCNF aig l path =
  withNetworkPtr (AIG.Network aig [l]) $ \pNtk -> do
    withAbcNtkToDar pNtk False False $ \pMan -> do
      vars <- writeAIGManToCNFWithMapping pMan path
      ciCount <- aigManCiNum pMan
      forM [0..(ciCount - 1)] $ \i -> do
        ci <- aigManCi pMan (fromIntegral i)
        ((vars V.!) . fromIntegral) `fmap` (aigObjId ci)

-- | Convert the network referred to by an AIG manager into CNF format
-- and write to a file, returning a mapping from ABC object IDs to CNF
-- variable numbers.
writeAIGManToCNFWithMapping :: Aig_Man_t -> FilePath -> IO (V.Vector Int)
writeAIGManToCNFWithMapping pMan path =
  withCnfDerive pMan 0 $ \pCnf -> do
    cnfDataWriteIntoFile pCnf path 1 nullPtr nullPtr
    getCNFMapping pMan pCnf

-- | Return the array mapping AIG network objects (indices) to CNF
-- variable numbers (entries) for a CNF data object derived from the
-- given AIG manager.
getCNFMapping :: Aig_Man_t -> Cnf_Dat_t -> IO (V.Vector Int)
getCNFMapping pMan pCnf = do
    objCount <- fmap fromIntegral $ aigManObjNumMax pMan
    varsPtr <- cnfVarNums pCnf
    V.generateM objCount $ \i -> fromIntegral <$> peekElemOff varsPtr i
