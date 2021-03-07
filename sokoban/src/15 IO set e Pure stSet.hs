module Main where

import Sokoban
import Rep0

import System.Process (system)
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime (..))

import Control.Concurrent.Async
import Control.Exception (finally, bracket)


-----------------------------------------------------
-----------------------------------------------------
solver :: UTCTime -> State -> Int -> Int -> IO (Maybe State)
solver t0 st p1 p2 = do
    set    <- newMyVar (eOrd st)
    bfs 1 set [st] 
   
  where -- escopo solver
    bfs iter set sts  = do

        debug iter t0 sts sts
        
        if iter < 50 then do
        
            r <- loop ts 
            case r of
                (Just x, _)   -> return (Just x)
                (Nothing, stSet') -> do
                    -- sts'    <- listMyVar stSet
                    -- stSet'  <- newMyVar st
                    bfs iter' set (tolist stSet') 
        else 
            return Nothing

      where  -- escopo bfs
        iter' = iter + 1
        ts    = [(m, s) | s  <- sts, m  <- actions]





        ----------------------------------------------------
        -- loop :: [(Moves, State)] -> IO (Maybe State)
        loop m = do
            semaf <- newSemaf p1
            -- stSet <- newMyVar st
            let stSet = insert Empty st
            go stSet semaf (particiona (p2+1) m) []
            
          where -- escopo loop de nivel
            
            go stSet semaf [] = waitLoop stSet
              where
                waitLoop stSet' [] = do 
                    return (Nothing, stSet')
                
                waitLoop stSet (xs:xss) = do
                    r <- wait xs
                    case r of
                        (Just cm, _) -> return r
                        (Nothing, stSet') -> waitLoop (merge stSet stSet') xss 


            go stSet semaf (xs:xss) = \asyncs -> do
                
                q <- obtemTicket semaf
                if q then do

                    let res = (subloop stSet xs) `finally` (liberaSemaf semaf)
                    
                    withAsync res $ \a -> 
                        go stSet semaf xss (a:asyncs) 

                else do 
                    r <- subloop stSet xs
                    case r of
                        (Just cm, _) -> return r
                        (Nothing, stSet') -> go stSet' semaf xss asyncs

              where
                subloop stSet m = go stSet m  where
                    go stSet []     = return (Nothing, stSet)
                    go stSet ((m, s):xs) = do
                        let r = funcSucess m s
                        case r of
                            Goal cm     -> return (Just cm, stSet)
                            Invalid     -> go stSet xs
                            Factible cm -> do

                                look <- lookupMyVar set (eOrd cm)
                                if look then 
                                    go stSet xs
                                else do
                                
                                    putMyVar set (eOrd cm)
                                    go (insert stSet cm) xs
                                







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






ret0f :: UTCTime -> IO (Maybe State)
ret0f t0 = do
    solver t0 testSt 24 24

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




-- 11.7 - Programação Funcional em Haskell: Monad State
-- https://youtu.be/_yKJ2ft9Lg4?list=PLYItvall0TqJ25sVTLcMhxsE0Hci58mpQ






newSemaf2 :: e -> ST e ()
newSemaf2 n = ST (\m -> ((), n))


obtemTicket2 :: ST Int Bool
obtemTicket2 = ST cont where
    cont m | m == 0    = (False, 0)
           | otherwise = (True, m - 1)
           
liberaSemaf2 :: ST Int ()
liberaSemaf2 = ST (\m -> ((), m + 1))



--------------------------------------------
--------------------------------------------
newtype ST e a = ST {runstate :: (e -> (a, e))}
--------------------------------------------
instance Monad (ST e) where
   ST g >>= h = ST (\e ->
                    let (x, e') = g e
                        ST f = h x
                        
                    in  f e')
--------------------------------------------
instance Applicative (ST e)  where
    pure a = ST (\e -> (a, e))
    stf <*> stx = do
                    f <- stf
                    fmap f stx
--------------------------------------------
instance Functor (ST s) where
    fmap f st = do
                  x <- st
                  return (f x)
--------------------------------------------
--------------------------------------------


