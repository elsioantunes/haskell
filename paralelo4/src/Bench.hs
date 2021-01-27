module Bench where
-- stack exec bench -- --output range.htm

import Criterion.Main
import qualified Solver as S

main :: IO()
main = defaultMain [
            bgroup "solver0f" [teste (i*i) | i <- [0..5]]
      ]
  
  where
    teste i = bench (show i) $ nfIO $ S.taskIO  i
    
    
    

