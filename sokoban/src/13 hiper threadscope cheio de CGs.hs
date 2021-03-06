module Main where

import Sokoban
import Rep0

import System.Process (system)
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime (..))

import Control.Concurrent.Async
import Control.Exception (finally, bracket)


-----------------------------------------------------
-----------------------------------------------------
-- solver :: UTCTime -> State -> IO (Maybe State)
solver t0 st p1 p2 = do
    set    <- newMyVar (eOrd st)
    stSet  <- newMyVar st
    semaf  <- newSemaf p1
    
    bfs 1 set stSet [st] semaf
   
  where -- escopo solver
    bfs iter set stSet sts semaf = do

    --  if iter > 50 then
    --      return Nothing
    --  else do
            -- debug iter t0 sts ts

            r <- loop ts 
            case r of
                Just x  -> return (Just x)
                Nothing -> do
                    sts'    <- listMyVar stSet
                    stSet'  <- newMyVar st
                    bfs iter' set stSet' sts' semaf

      where  -- escopo bfs
        iter' = iter + 1
        ts    = [(m, s) | s  <- sts, m  <- actions]




        ----------------------------------------------------
        loop m = go (particiona (p2 + 1) m) []  where
            go []     = waitLoop
            go (xs:xss) = \asyncs -> do
                
                q <- obtemTicket semaf
                if q then do

                    let res = (subloop xs) `finally` (liberaSemaf semaf)
                    withAsync res $ \a -> 
                        go xss (a:asyncs) 

                else do 
                    r <- subloop xs
                    case r of
                        Just cm -> return r
                        Nothing -> go xss asyncs

        ----------------------------------------------------
        waitLoop = go where 
            go []     = return Nothing
            go (xs:xss) = do
                r <- wait xs
                case r of
                    Just cm -> return r
                    Nothing -> go xss 

        ----------------------------------------------------
        subloop m = go m  where
            go []     = return Nothing
            go (x:xs) = do
                r <- resolve x
                case r of
                    Goal cm     -> return (Just cm)
                    Invalid     -> go xs
                    Factible cm -> putMyVar stSet cm >> go xs 
                        
            resolve :: (Moves, State) -> IO (MovType State)
            resolve (m, s) = filtrVisit =<< funcSucess m s
                        
        ----------------------------------------------------
        filtrVisit :: MovType State -> IO (MovType State)
        filtrVisit r = case r of 
            Invalid     -> return Invalid
            Goal cm     -> return r
            Factible cm -> do
                look <- lookupMyVar set (eOrd cm)
                if look then return Invalid
                else do
                    putMyVar set (eOrd cm)
                    return r

        ----------------------------------------------------






actions :: [Moves]
actions = [toEnum 0 ..]
    
particiona :: Int -> [a] -> [[a]]
particiona n m = go m n where
    go _ 0 = []
    go m n = a : go b (n-1) where
        (a, b) = splitAt (length m `div` n) m



















-----------------------------------------------------
-----------------------------------------------------
debug iter t0 sts sts' = do
    a <- getCurrentTime
    print (iter, diffUTCTime a t0, length sts, length sts')






-----------------------------------------------------
-----------------------------------------------------
ret0f t0 = do
    -- solver t0 testSt 18 2
    solver t0 testSt 18 2
--------------------------
main :: IO ()
main = do
        system "clear"
        t0 <- getCurrentTime
        r <- ret0f t0
        print r
        t1 <- getCurrentTime
        print (diffUTCTime t1 t0)
-----------------------------------------------------
taskIO (p1, p2) = do
    t0 <- getCurrentTime
    solver t0 testSt p1 p2
    t1 <- getCurrentTime
    print (diffUTCTime t1 t0)
    

-----------------------------------------------------







{----------------------------------------------------
    Arvores podem ser usadas para representar conjuntos em uma classe.
    E essa classe pode ser usada para representar 
    mapas ou hash tables, entre outras estruturas de dados.
    
    ...enumerando ou aplicando a fun??o de interesse
    ...bfs usando fila tem complexidade O(n?)  o ideal seria O(n)
    ...? possivel compor uma fila com tempo constante
----------------------------------------------------}

bfnJG t = t' where
    go (ks, Empty)        = (ks, Empty)
    go (k:ks0, Bin x a b) = (k+1 : ks2, Bin (x, k) a' b')
        where
            (ks1, a') = go (ks0, a)
            (ks2, b') = go (ks1, b)

    (ks, t')              = go (1:ks, t)


teste3 = bfnJG (Bin 'a' (Bin 'a' Empty Empty) (Bin 'a' Empty Empty))
