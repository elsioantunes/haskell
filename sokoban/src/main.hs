module Main where

import Sokoban
import Rep0

import System.Process (system)
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime (..))

import Control.Concurrent.Async
import Control.Exception (finally, bracket)


-----------------------------------------------------
-----------------------------------------------------
type Jobs     = [(Moves, State)]
type Box      = (Maybe State, Set State)
type Pacote = Set Jobs

solver :: UTCTime -> State -> Int -> IO (Maybe State)
solver t0 st p2 = do
    visit    <- newMyVar (eOrd st)
    bfs 1 visit [st] 
   
  where -- escopo solver
    bfs iter visit sts  = do

        -- debug
        debug iter t0 jobs sts
        if iter > 150 then do return Nothing 
        else do 

            -- main loop
            r <- loop jobs
            case r of
                (Just x, _)   -> return (Just x)
                (Nothing, stSet') -> bfs iter' visit (tolist stSet') 

      where  -- escopo bfs
        iter' = iter + 1
        jobs  = [(m, s) | s  <- sts, m  <- actions]

        ----------------------------------------------------
        loop :: Jobs -> IO Box
        loop m = asyncLoop (setNew st) (particiona2 p2 m)
          where
            asyncLoop :: Set State -> Pacote -> IO Box
            asyncLoop stSet = go Empty
               where
                go :: Set (Async Box) -> Pacote -> IO Box
                go asyncs Empty       =  waitLoop stSet asyncs
                go asyncs (Bin xs esq dir) =  
                    withAsync (subloop stSet xs) $ \a -> 
                        go (insert asyncs a) (merge esq dir)
                        

                subloop :: Set State -> Jobs -> IO Box
                subloop stSet' []     = return (Nothing, stSet')
                subloop stSet (x:xs) = do
                    case (funcSucess x) of
                        Goal cm     -> return (Just cm, Empty)
                        Invalid     -> subloop stSet xs
                        Factible cm -> do

                            look <- lookupMyVar visit (eOrd cm)
                            if look then 
                                subloop stSet xs
                            else do

                                putMyVar visit (eOrd cm)
                                subloop (insert stSet cm) xs

                waitLoop :: Set State -> Set (Async Box) -> IO Box
                waitLoop stSet' Empty = return (Nothing, stSet')
                waitLoop stSet (Bin xs esq dir) = do
                    r <- wait xs
                    case r of
                        (Just cm, _) -> return r
                        (Nothing, stSet') -> do 
                            let stSet'' = merge stSet stSet'
                            waitLoop stSet'' esq
                            waitLoop stSet'' dir 

        {----------------------------------------------------
        loop :: Jobs -> IO Box
        loop m = asyncLoop (setNew st) (particiona p2 m)
          where
            asyncLoop :: Set State -> Pacote -> IO Box
            asyncLoop stSet = go []
               where
                go asyncs []       =  waitLoop stSet asyncs
                go asyncs (xs:xss) =  
                    withAsync (subloop stSet xs) $ \a -> 
                        go (a:asyncs) xss 
                        

                subloop :: Set State -> Jobs -> IO Box
                subloop stSet' []     = return (Nothing, stSet')
                subloop stSet (x:xs) = do
                    case (funcSucess x) of
                        Goal cm     -> return (Just cm, Empty)
                        Invalid     -> subloop stSet xs
                        Factible cm -> do

                            look <- lookupMyVar visit (eOrd cm)
                            if look then 
                                subloop stSet xs
                            else do

                                putMyVar visit (eOrd cm)
                                subloop (insert stSet cm) xs

                waitLoop :: Set State -> [Async Box] -> IO Box
                waitLoop stSet' [] = return (Nothing, stSet')
                waitLoop stSet (xs:xss) = do
                    r <- wait xs
                    case r of
                        (Just cm, _) -> return r
                        (Nothing, stSet') -> 
                            waitLoop (merge stSet stSet') xss 

        ----------------------------------------------------}









actions :: [Moves]
actions = [toEnum 0 ..]
    
particiona :: Int -> [a] -> [[a]]
particiona n m = go m n where
    go _ 0 = []
    go m n = a : go b (n-1) where
        (a, b) = splitAt (length m `div` n) m

particiona2 n m = foldr func Empty (particiona n m) 
  where
    func x goxs = insert goxs x
    















-----------------------------------------------------
-----------------------------------------------------
debug iter t0 sts sts' = do
    a <- getCurrentTime
    print (iter, diffUTCTime a t0, length sts, length sts')






ret0f :: UTCTime -> IO (Maybe State)
ret0f t0 = do
    solver t0 testSt 24

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

taskIO p2 = do
    t0 <- getCurrentTime
    solver t0 testSt p2
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


