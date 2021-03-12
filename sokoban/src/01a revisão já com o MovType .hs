module Main where
import System.Process (system)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Concurrent
import Rep0
import Sokoban

-----------------------------------------------------
solver :: State -> IO (Maybe State)
solver st = do
    let set = setNew st
    t0 <- getCurrentTime
    bfs t0 1 set 

  where 
    bfs t0 iter set = do

        debug t0 iter jobs
        
        r <- loopIO set jobs 
        case r of
            (Nothing, set') -> bfs t0 iter' set'
            (Just x, _)  -> return (Just x)

      where    
        iter' = iter + 1
        sts   = tolist set
        jobs  = [(m, s) | s  <- sts, m  <- actions]

    actions :: [Moves]
    actions = [toEnum 0 ..]


    loopIO :: Set State -> [(Moves, State)] -> IO (Maybe State, Set State)
    loopIO set = go Empty where
        go set' []     = return (Nothing, set')
        go set' (x:xs) = do
            let r = funcSucess x
            case r of
                Goal cm     -> return (Just cm, Empty)
                Factible cm -> go (insert set' cm) xs
                Invalid     -> go set' xs
                

    

main = print "bla"


debug t0 iter jobs = do
    a <- getCurrentTime
    print (iter, diffUTCTime a t0, length jobs)


{-----------------------------------------------------
-----------------------------------------------------
ret0f t0 = do
    solver t0 testSt
--------------------------
main :: IO ()
main = do
        system "clear"
        t0 <- getCurrentTime
        r <- ret0f t0
        print r
        t1 <- getCurrentTime
        print (diffUTCTime t1 t0)
-----------------------------------------------------}
-----------------------------------------------------