module Main where

import Data.ABC.AIG
import Data.ABC.CNF
import System.Environment

main :: IO ()
main = do
  [aigPath, cnfPath] <- getArgs
  aig <- readAiger aigPath
  vars <- writeAIGToCNF aig cnfPath
  print vars
