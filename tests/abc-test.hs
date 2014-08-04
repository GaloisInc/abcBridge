{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Test.Framework

import qualified Data.ABC as ABC

import Tests.Basic
import Tests.Operations
import Tests.QBF

------------------------------------------------------------------------
-- Runner
------------------------------------------------------------------------

main :: IO ()
main = do
  ABC.initialize
  defaultMain tests

tests :: [Test.Framework.Test]
tests =
    [ testGroup "AIG basic" $ basic_tests ABC.aigNetwork
    , testGroup "AIG operations" $ op_tests ABC.giaNetwork
    , testGroup "GIA basic" $ basic_tests ABC.giaNetwork
    , testGroup "GIA operations" $ op_tests ABC.giaNetwork
    , testGroup "GIA QBF tests" $ qbf_tests
    ]

{-
tests :: [Test.Framework.Test]
tests =
    [ testGroup "AIG" $ do
       basic_tests ABC.aigNetwork ++ aig_tests
    , testGroup "GIA" $ basic_tests ABC.giaNetwork
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

basic_tests :: ABC.Proxy l g -> [Test.Framework.Test]
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
  , testCase "test_sat" $ do
     ABC.SomeGraph g <- ABC.newGraph proxy
     rt <- ABC.checkSat g (ABC.trueLit g)
     case rt of
       ABC.Sat{} -> return ()
       ABC.Unsat{} -> fail "trueLit is unsat"
     rf <- ABC.checkSat g (ABC.falseLit g)
     case rf of
       ABC.Sat{} -> fail "falseLit is sat"
       ABC.Unsat{} -> return ()
  , testProperty "test_add" $ \(i0 :: Int8) (i1 :: Int8) -> ioProperty $ do
      ABC.SomeGraph g <- ABC.newGraph proxy
      is0 <- replicateM 8 $ ABC.newInput g
      is1 <- replicateM 8 $ ABC.newInput g
      os <- Ops.add g (litsToBV is0) (litsToBV is1)
      let n = ABC.Network g (Ops.bvToList os)
      r <- ABC.evaluate n (bitsOfInt8 i0 ++ bitsOfInt8 i1)
      return $ bitsToInt8 r == i0 + i1
  , testProperty "test_sub" $ \(i0 :: Int8) (i1 :: Int8) -> ioProperty $ do
      ABC.SomeGraph g <- ABC.newGraph proxy
      is0 <- replicateM 8 $ ABC.newInput g
      is1 <- replicateM 8 $ ABC.newInput g
      os <- Ops.sub g (litsToBV is0) (litsToBV is1)
      let n = ABC.Network g (Ops.bvToList os)
      r <- ABC.evaluate n (bitsOfInt8 i0 ++ bitsOfInt8 i1)
      return $ bitsToInt8 r == i0 - i1
  , testProperty "test_neg" $ \(i0 :: Int8) -> ioProperty $ do
      ABC.SomeGraph g <- ABC.newGraph proxy
      is0 <- replicateM 8 $ ABC.newInput g
      os <- Ops.neg g (litsToBV is0)
      let n = ABC.Network g (Ops.bvToList os)
      r <- ABC.evaluate n (bitsOfInt8 i0)
      return $ bitsToInt8 r == (- i0)
  ]

singletonBV :: l -> Ops.BV l
singletonBV = Ops.replicate 1

litsToBV :: [l] -> Ops.BV l
litsToBV = Ops.concat . map singletonBV

bitsOfInt8 :: Int8 -> [Bool]
bitsOfInt8 n = unfoldr f 7
  where f i | i >= 0 = Just (testBit n i, i - 1)
            | otherwise = Nothing

bitsToInt8 :: [Bool] -> Int8
bitsToInt8 = foldr f 0
  where f b n = (shiftL n 1) .|. fromIntegral (fromEnum b)

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

aig_tests :: [Test.Framework.Test]
aig_tests =
  [ testCase "bad_aiger" $ do
      me <- tryIO $ ABC.readAigerAsAIG "Nonexistent AIGER!"
      case me of
        Left{} -> return ()
        Right{} -> fail "Expected error when opening AIGER"
  ]

-}