{-# LANGUAGE Rank2Types #-}
module Tests.Operations
  ( op_tests
  ) where

import qualified Data.AIG as AIG
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

bv :: AIG.IsAIG l g => g s -> Int -> Integer -> AIG.BV (l s)
bv = AIG.bvFromInteger

bin_test :: String
         -> AIG.Proxy l g
            -- ^ Proxy
         -> (forall s . g s -> AIG.BV (l s) -> AIG.BV (l s) -> IO (AIG.BV (l s)))
            -- ^ Bitvector operation
         -> (Integer -> Integer -> Integer)
            -- ^ Concrete op
         -> Test.Framework.Test
bin_test nm proxy@(AIG.Proxy f) bv_op c_op = f $
  testProperty nm $ \u v -> ioProperty $ do
    let w = 10
    AIG.SomeGraph g <- AIG.newGraph proxy
    z <- bv_op g (bv g w u) (bv g w v)
    let expected = (c_op u v) `mod` (2^w)
    return $ Just expected == AIG.asUnsigned g z

op_tests :: AIG.Proxy l g -> [Test.Framework.Test]
op_tests proxy@(AIG.Proxy f) = f $
  [ testProperty "test_bv" $ \u -> ioProperty $ do
      AIG.SomeGraph g <- AIG.newGraph proxy
      let w = 10
      let z = (bv g w u)
      let expected = u `mod` (2^w)
      return $ Just expected == AIG.asUnsigned g z
  , bin_test "test_add" proxy AIG.add (+)
  , bin_test "test_sub" proxy AIG.sub (-)
  , bin_test "test_mul" proxy AIG.mul (*)
  , testProperty "test_neg" $ \u -> ioProperty $ do
      let w = 10
      AIG.SomeGraph g <- AIG.newGraph proxy
      z <- AIG.neg g (bv g w u)
      let expected = (negate u) `mod` (2^w)
      return $ Just expected == AIG.asUnsigned g z
  ]