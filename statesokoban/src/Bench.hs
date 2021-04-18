module Bench where
-- stack exec bench -- --output range.htm

import Criterion.Main
import qualified Asynkoban as P

main :: IO()
main = defaultMain [
            bgroup "reduce" [teste2 (i*i*4) | i <- [1..4]],
            bgroup "pipe" [teste1 (i*i*4) | i <- [1..4]]
      ]
  
  where
    teste1 n = bench (show n) $ nfIO $ P.taskIO 1 n
    teste2 n = bench (show n) $ nfIO $ P.taskIO 2 n
    
    
    

