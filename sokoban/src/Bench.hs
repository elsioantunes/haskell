module Bench where
-- stack exec bench -- --output range.htm

-- main :: IO()
-- main = print 666

----------------------------------------------------
import Criterion.Main
import qualified Main as P

main :: IO()
main = defaultMain [
            -- bgroup "pipe8" [teste1 ((i*i)) | i <- [1..10]],
            -- bgroup "reduce2" [teste0 ((i*i)) | i <- [2,3,4]],
            bgroup "sokoban" [teste2 (7*a,7*b) | a <- [0..1], b <- [0..1],  (a - b) > -2],
            bgroup "sokoban" [teste2 (5*a,5*b) | a <- [4..7], b <- [4..7],  (a - b) > -2]
      ]
  
  where
    -- teste0 i = bench (show i) $ nfIO $ P.taskIO0  i
    -- teste1 i = bench (show i) $ nfIO $ P.taskIO1  i 
    teste2 i = bench (show i) $ nfIO $ P.taskIO i
    
    
    

{----------------------------------------------------}
