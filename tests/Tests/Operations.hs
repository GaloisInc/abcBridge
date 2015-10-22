{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Tests.Operations
  ( op_tests
  ) where

import qualified Data.AIG as AIG
import Test.Tasty
import Test.QuickCheck
import Test.Tasty.QuickCheck

import qualified Data.Bits as Bits
import qualified Data.Bits.Compat as Bits

bv :: AIG.IsAIG l g => g s -> Int -> Integer -> AIG.BV (l s)
bv = AIG.bvFromInteger

bin_test :: String
         -> AIG.Proxy l g
            -- ^ Proxy
         -> (forall s . g s -> AIG.BV (l s) -> AIG.BV (l s) -> IO (AIG.BV (l s)))
            -- ^ Bitvector operation
         -> (Integer -> Integer -> Integer)
            -- ^ Concrete op
         -> TestTree
bin_test nm proxy@(AIG.Proxy f) bv_op c_op = f $
  testProperty nm $ \u v -> ioProperty $ do
    let w = 10
    AIG.SomeGraph g <- AIG.newGraph proxy
    z <- bv_op g (bv g w u) (bv g w v)
    let expected = (c_op (u `mod` (2^w)) (v `mod` (2^w))) `mod` (2^w)
    return $ Just expected == AIG.asUnsigned g z

unary_test :: String
           -> AIG.Proxy l g
            -- ^ Proxy
         -> (forall s . g s -> AIG.BV (l s) -> IO (AIG.BV (l s)))
            -- ^ Bitvector operation
         -> (Integer -> Integer)
            -- ^ Concrete op
         -> TestTree
unary_test nm proxy@(AIG.Proxy f) bv_op c_op = f $
  testProperty nm $ \u -> ioProperty $ do
    let w = 10
    AIG.SomeGraph g <- AIG.newGraph proxy
    z <- bv_op g (bv g w u)
    let expected = (c_op (u `mod` (2^w))) `mod` (2^w)
    return $ Just expected == AIG.asUnsigned g z

-- reference implementations of lg2 and lg2_up
lg2 :: Integer -> Integer
lg2 = go 0
 where go i x
         | x <= 0    = -1
         | x == 1    = i
         | otherwise = go (i+1) (x `div` 2)

lg2_up :: Int -> Integer -> Integer
lg2_up w 0 = toInteger w
lg2_up _ x = lg2 (x-1) + 1


test_lg2_down :: AIG.Proxy l g
            -- ^ Proxy
         -> TestTree
test_lg2_down proxy@(AIG.Proxy f) = f $
  testProperty "test_lg2_down" $ \(w0::Int) u0 -> ioProperty $ do
    let w = ((abs w0) + 1)
    let u' = (abs u0)
    AIG.SomeGraph g <- AIG.newGraph proxy
    z <- AIG.logBase2_down g (bv g w u')
    let expected = (lg2 (u' `mod` (2^w))) `mod` (2^w)
    return $ Just expected == AIG.asUnsigned g z

test_lg2_up :: AIG.Proxy l g
            -- ^ Proxy
         -> TestTree
test_lg2_up proxy@(AIG.Proxy f) = f $
  testProperty "test_lg2_up" $ \(w0::Int) u0 -> ioProperty $ do
    let w = ((abs w0) + 1)
    let u' = (abs u0)
    AIG.SomeGraph g <- AIG.newGraph proxy
    z <- AIG.logBase2_up g (bv g w u')
    let expected = (lg2_up w (u' `mod` (2^w))) `mod` (2^w)
    return $ Just expected == AIG.asUnsigned g z

#if MIN_VERSION_base(4,8,0)
test_clz :: AIG.Proxy l g
            -- ^ Proxy
         -> TestTree
test_clz proxy@(AIG.Proxy f) = f $
  testProperty "test_clz" $ \(u::Int) -> ioProperty $ do
    let w = Bits.finiteBitSize u
    AIG.SomeGraph g <- AIG.newGraph proxy
    z <- AIG.countLeadingZeros g (bv g w (toInteger u))
    let expected = toInteger $ Bits.countLeadingZeros u
    return $ Just expected == AIG.asUnsigned g z

test_ctz :: AIG.Proxy l g
            -- ^ Proxy
         -> TestTree
test_ctz proxy@(AIG.Proxy f) = f $
  testProperty "test_ctz" $ \(u::Int) -> ioProperty $ do
    let w = Bits.finiteBitSize u
    AIG.SomeGraph g <- AIG.newGraph proxy
    z <- AIG.countTrailingZeros g (bv g w (toInteger u))
    let expected = toInteger $ Bits.countTrailingZeros u
    return $ Just expected == AIG.asUnsigned g z
#endif

op_tests :: AIG.Proxy l g -> [TestTree]
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
  , unary_test "test_neg" proxy AIG.neg negate
  , test_lg2_down proxy
  , test_lg2_up proxy
#if MIN_VERSION_base(4,8,0)
  , test_clz proxy
  , test_ctz proxy
#endif
  ]
