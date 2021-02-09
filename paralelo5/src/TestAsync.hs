module TestAsync where
import System.Process (system)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Control.Concurrent.Async
import Sokoban
import qualified Pipe as P




{----------------------------------------------------
    Versão BFS IO 
----------------------------------------------------
type Caminho = [Moves]
type Resp = Maybe Caminho
solver0e :: State -> IO Resp
solver0e st = (fmap . fmap) inverte (bfs [[]])  
  where
    bfs paths = do
        r <- loopIO paths'
        case r of
            Nothing -> bfs paths'
            Just _  -> return r

      where
        paths' = [(m:pat) | pat <- paths, m <- [toEnum 0 ..]]
        --------------------------
        predic :: Caminho -> IO Bool
        predic x = return (go x)  where
            go = (st ==) . pat2st

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
    solver0e testSt1


pat2st :: [Moves] -> State 
pat2st ms = (foldr ((.) . move) id ms) initSt


    
--------------------------}






-----------------------------------------------------
-----------------------------------------------------
main :: IO ()
main = do
        system "clear"
        t0 <- getCurrentTime

        -- r <- ret0e
        -- print r
        
        t1 <- getCurrentTime
        print (diffUTCTime t1 t0)
-----------------------------------------------------
-----------------------------------------------------


