module Tests.QBF
  ( qbf_tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ABC as ABC
import qualified Data.ABC.GIA as GIA

qbf_tests :: [TestTree]
qbf_tests =
  [ testCase "test_qbf_and" $ do
      ABC.SomeGraph g <- GIA.newGIA
      i0 <- ABC.newInput g
      i1 <- ABC.newInput g
      x <- ABC.and g i0 i1
      Right r <- GIA.check_exists_forall g 1 x [False] 1000
      case r of
        ABC.Sat{} -> fail "abc returned sat"
        ABC.Unsat{} -> return ()
  , testCase "test_qbf_or" $ do
      ABC.SomeGraph g <- GIA.newGIA
      i0 <- ABC.newInput g
      i1 <- ABC.newInput g
      x <- ABC.or g i0 i1
      Right r <- GIA.check_exists_forall g 1 x [False] 1000
      case r of
        ABC.Sat [True] -> return ()
        _ -> fail "abc returned bad value."
  , testCase "test_qbf_implies" $ do
      ABC.SomeGraph g <- GIA.newGIA
      i0 <- ABC.newInput g
      i1 <- ABC.newInput g
      x <- ABC.implies g i0 i1
      Right r <- GIA.check_exists_forall g 1 x [False] 1000
      case r of
        ABC.Sat [False] -> return ()
        _ -> fail "abc returned bad value."
  , testCase "test_qbf_zero_iterations" $ do
      ABC.SomeGraph g <- GIA.newGIA
      i0 <- ABC.newInput g
      i1 <- ABC.newInput g
      x <- ABC.implies g i0 i1
      Left _msg <- GIA.check_exists_forall g 1 x [False] 0
      return ()
  ]