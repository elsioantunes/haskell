module Bench where
-- stack exec bench -- --output range.htm

import Criterion.Main
import qualified Pipe as P
import qualified Reduce as R


main :: IO()
main = do
        print "Nothing in here!"
{-
main :: IO()
main = defaultMain [
        bgroup cmdtxt1 [
            bench (show param1) $ nfIO $ 
                P.taskIO  (cmdtxt1, show param1)  (P.pipe1 [1..param1])
        ],

        bgroup cmdtxt2 [
            bench (paramtxt2 p) $ nfIO $ 
                R.taskIO  (cmdtxt2, paramtxt2 p) (R.reduce2 p) 
                    | p <- R.params
        ]
      ]
  
  where
    (cmdtxt1, cmdtxt2) = ("pipe1", "reduce2")
    paramtxt2 i = show i
    param1 = 10000000
    
    
    

-}