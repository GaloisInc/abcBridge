module Tests.Basic
  ( basic_tests
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import System.Directory
import System.IO

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck

import qualified Data.ABC as ABC
import qualified Data.AIG.Trace as Tr


tryIO :: IO a -> IO (Either IOException a)
tryIO = try

basic_tests :: Tr.Traceable l => ABC.Proxy l g -> [TestTree]
basic_tests proxy@(ABC.Proxy f) = f $
  [ testCase "test_true" $ do
      ABC.SomeGraph g <- ABC.newGraph proxy
      let n = ABC.Network g [ABC.trueLit g]
      assertEqual "test_true" [True] =<< ABC.evaluate n []
  , testCase "test_false" $ do
      ABC.SomeGraph g <- ABC.newGraph proxy
      let n = ABC.Network g [ABC.falseLit g]
      assertEqual "test_false" [False] =<< ABC.evaluate n []
  , testProperty "test_constant"$ \b -> ioProperty $do
      ABC.SomeGraph g <- ABC.newGraph proxy
      let n = ABC.Network g [ABC.constant g b]
      (==[b]) <$> ABC.evaluate n []
  , testProperty "test_not" $ \b0 -> ioProperty $ do
      ABC.SomeGraph g <- ABC.newGraph proxy
      i0 <- ABC.newInput g
      let n = ABC.Network g [ABC.not i0]
      r <- ABC.evaluate n [b0]
      return $ r == [not b0]
  , testProperty "test_and" $ \b1 b2 -> ioProperty $ do
      ABC.SomeGraph g <- ABC.newGraph proxy
      i0 <- ABC.newInput g
      i1 <- ABC.newInput g
      x <- ABC.and g i0 i1
      let n = ABC.Network g [x]
      r <- ABC.evaluate n [b1, b2]
      return $ r == [b1 && b2]
  , testProperty "test_xor" $ \b1 b2 -> ioProperty $ do
      ABC.SomeGraph g <- ABC.newGraph proxy
      i0 <- ABC.newInput g
      i1 <- ABC.newInput g
      x <- ABC.xor g i0 i1
      let n = ABC.Network g [x]
      r <- ABC.evaluate n [b1, b2]
      return $ r == [b1 /= b2]
  , testProperty "test_mux" $ \b0 b1 b2 -> ioProperty $ do
      ABC.SomeGraph g <- ABC.newGraph proxy
      i0 <- ABC.newInput g
      i1 <- ABC.newInput g
      i2 <- ABC.newInput g
      o <- ABC.mux g i0 i1 i2

      let n = ABC.Network g [o]
      r <- ABC.evaluate n [b0, b1, b2]
      return $ r == [if b0 then b1 else b2]
  , testCase "test_cec" $ do
     r <- join $ ABC.cec <$> cecNetwork proxy <*> cecNetwork' proxy
     assertEqual "test_cec" (ABC.Invalid (toEnum <$> [0,0,0,1,0,0,0])) r
  , testCase "test_aiger" $ do
      -- XXX: cwd unfriendly
      n1 <- ABC.aigerNetwork proxy "tests/eijk.S298.S.aig"
      tmpdir <- getTemporaryDirectory
      (path, hndl) <- openTempFile tmpdir "aiger.aig"
      hClose hndl
      ABC.writeAiger path n1
      n2 <- ABC.aigerNetwork proxy path
      assertEqual "test_aiger" ABC.Valid =<< ABC.cec n1 n2
      removeFile path

  , testProperty "unfold_fold" $ \litForest -> ioProperty $ do
      let maxInput = foldr max 0 $ map ABC.getMaxInput litForest

      n1@(ABC.Network g ls) <- ABC.buildNetwork proxy litForest

      litForest' <- ABC.toLitForest g ls

      -- NB: we cannot just compare litForest and litForest' for syntactic equality
      -- due to simplifications performed when building the AIG.  Also, the following
      -- commented line does not work because references to inputs may also be removed
      -- during simpification, resulting in a different number of inputs.
      --n2 <- ABC.buildNetwork proxy litForest'

      -- so do this instead...
      (ABC.SomeGraph g') <- ABC.newGraph proxy
      forM_ [0 .. maxInput] (\_ -> ABC.newInput g')
      ls' <- ABC.fromLitForest g' litForest'
      let n2 = ABC.Network g' ls'

      result <- ABC.cec n1 n2
      return $ result == ABC.Valid

  , testCase "fold_unfold" $ do
      (ABC.Network g l) <- cecNetwork proxy
      inputs <- ABC.inputCount g
      litForest <- ABC.toLitForest g l

      (ABC.SomeGraph g') <- ABC.newGraph proxy
      forM_ [0 .. inputs-1] (\_ -> ABC.newInput g')
      l' <- ABC.fromLitForest g' litForest

      assertEqual "fold_unfold" ABC.Valid =<< ABC.cec (ABC.Network g l) (ABC.Network g' l')

  , testCase "bad_aiger" $ do
      me <- tryIO $ ABC.aigerNetwork proxy "Nonexistent AIGER!"
      case me of
        Left{} -> return ()
        Right{} -> fail "Expected error when opening AIGER"
  , testCase "test_sat" $ do
     ABC.SomeGraph g <- ABC.newGraph proxy
     rt <- ABC.checkSat g (ABC.trueLit g)
     case rt of
       ABC.Sat{} -> return ()
       ABC.Unsat{} -> fail "trueLit is unsat"
       ABC.SatUnknown{} -> fail "trueLit is unknown"
     rf <- ABC.checkSat g (ABC.falseLit g)
     case rf of
       ABC.Sat{} -> fail "falseLit is sat"
       ABC.Unsat{} -> return ()
       ABC.SatUnknown{} -> fail "falseLit is unknown"

  , testCase "aiger_twice" $ do
      ABC.SomeGraph g <- ABC.newGraph proxy

      tmpdir <- getTemporaryDirectory
      (path, hndl) <- openTempFile tmpdir "aiger.aig"
      hClose hndl

      x <- ABC.newInput g

      ABC.writeAiger (path++"1") (ABC.Network g [ABC.falseLit g, ABC.falseLit g])

      y <- ABC.newInput g
      r <- ABC.and g x y

      ABC.writeAiger (path++"2") (ABC.Network g [r])

  , testCase "aiger_eval" $ do
      ABC.SomeGraph g <- ABC.newGraph proxy

      tmpdir <- getTemporaryDirectory
      (path, hndl) <- openTempFile tmpdir "aiger.aig"
      hClose hndl

      x <- fmap ABC.bvFromList $ sequence $ replicate 32 (ABC.newInput g)
      y <- ABC.zipWithM (ABC.lAnd' g) x (ABC.bvFromInteger g 32 0x12345678)

      ABC.writeAiger path (ABC.Network g (ABC.bvToList y))

      let tobool :: Int -> Bool
          tobool i = if i == 0 then False else True

      let inputs = map tobool $ reverse $
                   [ 0,1,1,0,1,0,0,0,
                     0,0,0,0,0,0,0,0,
                     0,0,0,0,0,0,0,0,
                     0,0,0,0,0,0,0,0 ]

      let outputs = fmap tobool $ reverse $
                    [ 0,0,0,0,1,0,0,0,
                      0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0 ]

      z <- ABC.evaluate (ABC.Network g (ABC.bvToList y)) inputs

      assertEqual "aiger_eval" outputs z
  ]

cecNetwork :: ABC.IsAIG l g => ABC.Proxy l g -> IO (ABC.Network l g)
cecNetwork proxy = do
  ABC.SomeGraph g <- ABC.newGraph proxy
  [n2, n3, n4, n5, n6, n7, n8] <- replicateM 7 $ ABC.newInput g

  n14 <- ABC.ands g [ ABC.not n2
                    , ABC.not n3
                    , ABC.not n4
                    , n5
                    , ABC.not n6
                    , ABC.not n7
                    , ABC.not n8
                    ]
  let r = [n14] ++ replicate 6 (ABC.falseLit g)
  return (ABC.Network g r)

cecNetwork' :: ABC.IsAIG l g => ABC.Proxy l g -> IO (ABC.Network l g)
cecNetwork' proxy = do
  ABC.SomeGraph g <- ABC.newGraph proxy
  replicateM_ 7 $ ABC.newInput g
  let r = replicate 7 $ ABC.falseLit g
  return (ABC.Network g r)
