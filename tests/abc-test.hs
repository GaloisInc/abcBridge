{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Runners.AntXML

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
  defaultMainWithIngredients ingrs tests

ingrs :: [Ingredient]
ingrs =
   [ antXMLRunner
   ]
   ++
   defaultIngredients


tests :: TestTree
tests =
    testGroup "ABC Bridge"
    [ testGroup "AIG basic" $ basic_tests ABC.aigNetwork
    , testGroup "AIG operations" $ op_tests ABC.aigNetwork
    , testGroup "GIA basic" $ basic_tests ABC.giaNetwork
    , testGroup "GIA operations" $ op_tests ABC.giaNetwork
    , testGroup "GIA QBF tests" $ qbf_tests
    ]
