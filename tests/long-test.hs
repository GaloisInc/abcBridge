import Control.Monad
import qualified Data.ABC as ABC
import Data.ABC.AIG (AIG, Lit, Network)
import qualified Data.ABC.AIG as AIG

main :: IO ()
main = do

  ABC.initialize

  n <- AIG.readAiger "tests/eijk.S298.S.aig"
  c <- AIG.networkInputCount n
  n2 <- constantNetwork c False

  putStrLn "Calling cec"
  print =<< AIG.cec n n2

constantNetwork :: Int -> Bool -> IO (Network Lit AIG)
constantNetwork i b = do
  ABC.SomeGraph n <- AIG.newAIG
  replicateM_ i (AIG.newInput n)
  return (AIG.Network n [AIG.constant n b])
