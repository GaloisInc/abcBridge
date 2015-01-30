module Main where

import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.QuickCheck

import qualified Data.ABC as ABC

import Tests.Basic
import Tests.Operations
import Tests.QBF

import System.Environment
import System.Posix.Process
import System.Posix.IO
import System.Posix.Files

import System.Random
import System.Random.TF
import System.Random.TF.Init

import Test.QuickCheck.Random

{-
This utility runs the test suite in a separate process with differently-chosen random seeds.
It continually runs the test suite until the child process exits on a signal.  We are trying to
track down an infrequently-occuring segfault.  The idea is to find a random seed that tickles
the error so we can run it in a repeatable way.
-}

------------------------------------------------------------------------
-- Runner
------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    [strSize, strNum] -> do
       let testsize = read strSize
       let numtests = read strNum
       gen <- initTFGen
       searchForSegfault gen testsize numtests
    [strSize, strNum, strSeed] -> do
       let testsize = read strSize
       let numtests = read strNum
       let seed = read strSeed
       withArgs [] (runTests seed testsize numtests)
    _ -> error "incorrect arguments"

searchForSegfault :: TFGen -> Int -> Int -> IO ()
searchForSegfault gen testsize numtests = do
  let (seed,gen') = next gen
  putStrLn $ unwords ["Running test with seed: ", show seed, "and size:", show testsize,"and number",show numtests]
  pid <- forkProcess (forkTests seed testsize numtests)
  putStrLn $ unwords ["child forked", show pid]
  st <- getProcessStatus True False pid
  print st
  case st of
     Nothing -> error "process status not avaliable?"
     Just (Exited cd) -> do
           putStrLn $ unwords ["Normal Exit", show cd]
           searchForSegfault gen' testsize numtests
     Just (Terminated sig _coreDumped) -> do
           putStrLn $ unwords ["Terminated by signal:", show sig]
     Just (Stopped sig) -> do
           putStrLn $ unwords ["Stopped by signal:", show sig]

forkTests :: Int -> Int -> Int -> IO ()
forkTests seed sz num = do
  fd <- openFd "child.output" WriteOnly (Just stdFileMode) defaultFileFlags{ trunc = True }
  _ <- dupTo fd stdOutput
  withArgs [] (runTests seed sz num)

runTests :: Int -> Int -> Int -> IO ()
runTests seed sz num = do
  ABC.initialize
  putStrLn $ unwords ["running tests in child process..."]
  defaultMainWithIngredients ingrs
      ( localOption (QuickCheckReplay (Just (mkQCGen seed, sz)))
      $ localOption (QuickCheckTests num)
      $ tests
      )

ingrs :: [Ingredient]
ingrs = defaultIngredients

tests :: TestTree
tests =
    testGroup "ABC Bridge"
    [ testGroup "AIG basic" $ basic_tests ABC.aigNetwork
    , testGroup "AIG operations" $ op_tests ABC.aigNetwork
    , testGroup "GIA basic" $ basic_tests ABC.giaNetwork
    , testGroup "GIA operations" $ op_tests ABC.giaNetwork
    , testGroup "GIA QBF tests" $ qbf_tests
    ]
