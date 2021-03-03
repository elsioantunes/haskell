module Main where

import Sokoban
import Rep0

import System.Process (system)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Control.Concurrent.Async
import Control.Exception (finally, bracket)


-----------------------------------------------------
-----------------------------------------------------
-- solver :: UTCTime -> State -> IO (Maybe State)
solver t0 st = do
    set    <- newMyVar (eOrd st)
    semaf  <- newSemaf 4
    bfs 1 set [st] semaf
   
  where -- escopo solver
    bfs iter set sts semaf = do
        debug iter t0 sts ts
        
        r <- subloop ts
        case r of
            (Just x, _)     -> return (Just x)
            (Nothing, sts') -> bfs iter' set sts' semaf

      where  -- escopo bfs
        iter' = iter + 1
        ts    = [(m, s) | s  <- sts, m  <- actions]

        resolve :: MovType State -> IO (MovType State)
        resolve r = case r of 
            Invalid     -> return Invalid
            Goal cm     -> return r
            Factible cm -> do
                look <- lookupMyVar set (eOrd cm)
                if look then return Invalid
                else do
                    putMyVar set (eOrd cm)
                    return r
----------------------------------------------------
        subloop m = go [] [] m where
            -- go :: [Async (MovType State)] -> [(Moves, State)] -> [State] -> IO (Maybe State, [State])
            go acc asyncs [] = waitLoop acc asyncs
            go acc asyncs (x:xs) =  do
                q <- obtemTicket semaf
                if q then do
                    let res = (uncurry funcSucess x) `finally` (liberaSemaf semaf)
                    withAsync res $ \a -> 
                        go acc (a:asyncs) xs 

                else do 
                    -- resolve1 :: (Moves, State) -> IO (MovType State)
                    r <- resolve =<< uncurry funcSucess x
                    case r of
                        Goal cm     -> return (Just cm, [])
                        Invalid     -> go acc      asyncs xs
                        Factible cm -> go (cm:acc) asyncs xs
----------------------------------------------------
        waitLoop = go where
            -- go :: [State] -> [Async (MovType State)] -> IO (Maybe State, [State])
            go acc []     = return (Nothing, acc)
            go acc (x:xs) = do
                -- resolve2 :: Async (MovType State) -> IO (MovType State)
                r <- resolve =<< wait x
                case r of
                    Goal cm     -> return (Just cm, [])
                    Invalid     -> go acc xs
                    Factible cm -> go (cm:acc) xs

----------------------------------------------------

actions :: [Moves]
actions = [toEnum 0 ..]
    



















-----------------------------------------------------
-----------------------------------------------------
-----------------------------------------------------
debug iter t0 sts sts' = do
    a <- getCurrentTime
    print (iter, diffUTCTime a t0, length sts, length sts')

-----------------------------------------------------
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
-----------------------------------------------------
-----------------------------------------------------







{----------------------------------------------------
    Arvores podem ser usadas para representar conjuntos em uma classe.
    E essa classe pode ser usada para representar 
    mapas ou hash tables, entre outras estruturas de dados.
    
    ...enumerando ou aplicando a função de interesse
    ...bfs usando fila tem complexidade O(n²)  o ideal seria O(n)
    ...é possivel compor uma fila com tempo constante
----------------------------------------------------}

bfnJG t = t' where
    go (ks, Empty)        = (ks, Empty)
    go (k:ks0, Bin x a b) = (k+1 : ks2, Bin (x, k) a' b')
        where
            (ks1, a') = go (ks0, a)
            (ks2, b') = go (ks1, b)

    (ks, t')              = go (1:ks, t)


teste3 = bfnJG (Bin 'a' (Bin 'a' Empty Empty) (Bin 'a' Empty Empty))

