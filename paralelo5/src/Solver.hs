module Solver where
import System.Process (system)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Control.Concurrent.Async
import Sokoban
    



----------------------------------------------------
----------------------------------------------------
type Caminho = [Moves]
type Resp = Maybe Caminho
solver0e :: State -> IO Resp
solver0e st = (fmap . fmap) reverse (bfs [[]])  
  where
    bfs paths = do
        r <- loopIO paths'
        case r of
            Nothing -> bfs paths'
            Just _  -> return r

      where
        paths' = [(m:pat) | pat <- paths, m <- [toEnum 0 ..]]
        
        uniq [] = []
        uniq (x:xs) = x:uniq (filter ((/=) x) xs)
        --------------------------
        predic :: Caminho -> IO Bool
        predic x = go (pat2st x)
          where
            go Nothing = return False 
            go (Just st) = return (isGoal st)

        --------------------------
        loopIO :: [Caminho] -> IO Resp
        loopIO = go
          where
            go [] = return Nothing
            go (x:xs) = do
                         r <- predic x
                         case r of 
                            True  -> return (Just x)
                            False -> go xs
--------------------------
ret0e :: IO Resp
ret0e = do
    solver0e testSt
--------------------------





-----------------------------------------------------
-----------------------------------------------------
main :: IO ()
main = do
        system "clear"
        t0 <- getCurrentTime

        r <- ret0e
        print r
        
        t1 <- getCurrentTime
        print (diffUTCTime t1 t0)
-----------------------------------------------------
-----------------------------------------------------


