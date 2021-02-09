module Bench where
-- stack exec bench -- --output range.htm

import Criterion.Main
import qualified Pipe as P

main :: IO()
main = defaultMain [
            -- bgroup "Serial" [bench "" $ nfIO $ P.taskIO2],
            bgroup "pipe8" [teste1 ((i*i)) | i <- [1..10]],
            bgroup "reduce2" [teste0 ((i*i)) | i <- [2,3,4]],
            -- bgroup "Reduce0" [teste0 (i) | i <- [1]],
            bgroup "pipe8" [teste2 ((i*i)) | i <- [2]]
      ]
  
  where
    teste0 i = bench (show i) $ nfIO $ P.taskIO0  i
    teste1 i = bench (show i) $ nfIO $ P.taskIO1  i
    teste2 i = bench (show i) $ nfIO $ P.taskIO2  i
    
    
    

