{-# LANGUAGE BangPatterns #-}
module Main where
import System.Process (system)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Concurrent.Async
import Sokoban

import Data.IORef (IORef (..), newIORef, readIORef, atomicModifyIORef) 
import Control.Concurrent (MVar (..), putMVar, takeMVar, newMVar)

import Control.Exception (finally, bracket)

-----------------------------------------------------
-----------------------------------------------------
solver t0 semaf st = do
    set <- newMyVar $ ordSt st
    bfs 1 set 
   
   where 
    bfs iter set = do
    
        sts <- listMyVar set
        
        let sts'= [(m, s) | s  <- sts, m  <- actions]
        
        debug iter t0 sts sts'
        
        r <- loopIO set sts' 
            
        case r of
            Nothing -> bfs iter' set
            Just x  -> return (Just x)

      where    
        iter' = iter + 1



loopIO set = go  where
    go []     = return Nothing
    go (x:xs) = do
        
        r <- uncurry funcSucess x
        
        case r of
            Goal cm     -> return (Just cm)
            Invalid     -> go xs
            Factible cm -> do
                r <- lookupMyVar set cm
                if r then 
                    go xs
                
                else do
                    putMyVar set cm
                    go xs
                    


actions :: [Moves]
actions = [toEnum 0 ..]
    



----------------------------------------------------
----------------------------------------------------}
data Set a = Bin a (Set a) (Set a) |  Empty deriving Show

setNew :: Ord a => a -> Set a
setNew x = do
    insert Empty x

tolist = go where
    go Empty = []
    go (Bin a esq dir) = go esq ++ go dir ++ [a]

updateSet :: Ord a => Set a -> [a] -> Set a
updateSet = foldl insert

insert :: Ord a => Set a -> a -> Set a
insert = insWith compare where
    
insWith :: (a -> a -> Ordering) -> Set a -> a -> Set a
insWith cmp set x = go x set where
    go x Empty = Bin x Empty Empty
    go x t@(Bin y esq dir) = 
        case cmp x y of 
            LT -> Bin y (go x esq) dir
            GT -> Bin y esq (go x dir)
            _  -> t

cmp1 :: Ord a1 => (a2, b1, a1) -> (a3, b2, a1) -> Ordering
cmp1 (_, _, bx) (_, _, by) 
    | bx < by = LT
    | bx > by = GT
    | otherwise = EQ


member set x = go x set where
    go x Empty = False
    go x (Bin y esq dir) = 
        case compare x y of 
            LT -> go x esq
            GT -> go x dir
            _  -> True
----------------------------------------------------
----------------------------------------------------











----------------------------------------------------
-- MyVar --------------------------------------------
newMyVar :: Ord a => a -> IO (MVar (Set a))
newMyVar a = do
    m <- newMVar (insert Empty a)
    return m

putMyVar :: Ord a => MVar (Set a) -> a -> IO ()
putMyVar m a = do
    set <- takeMVar m 
    putMVar m (insert set a) 

lookupMyVar :: Ord t => MVar (Set t) -> t -> IO Bool
lookupMyVar m a = do
    set <- takeMVar m
    putMVar m set
    return (member set a)


listMyVar :: MVar (Set a) -> IO [a]
listMyVar m = do
    set <- takeMVar m
    putMVar m set
    return (tolist set)


----------------------------------------------------
----------------------------------------------------

    
    
    
    
    
    
    
{----------------------------------------------------


obtemTicket1 :: Semaf -> IO Bool
obtemTicket1 m = atomicModifyIORef m sem
  where
    sem :: Int ->    (Int, Bool)    
    sem i | i == 0    = (i, False)
          | otherwise = let  
                          !z = i - 1
                        in 
                          (z, True)

liberaSemaf1 :: Semaf -> IO ()
liberaSemaf1 m = atomicModifyIORef m sem
  where
    sem :: Int ->    (Int, ())
    sem i  = let
               !z = i + 1 -- ? "avoid building up a large expression inside the IORef: 1 + 1 + 1 + ...." /Simon
             in
                (z, ())

----------------------------------------------------}
----------------------------------------------------





----------------------------------------------------
-- Semaforo ----------------------------------------
type Semaf = IORef Int 

newSemaf :: Int -> IO Semaf 
newSemaf i = do
    m <- newIORef i
    return m

obtemTicket :: Semaf -> IO Bool
obtemTicket m = atomicModifyIORef m sem
  where
    sem :: Int ->    (Int, Bool)    
    sem i | i == 0    = (i, False)
          | otherwise = let  
                          !z = i - 1
                        in 
                          (z, True)

liberaSemaf :: Semaf -> IO ()
liberaSemaf m = atomicModifyIORef m sem
  where
    sem :: Int ->    (Int, ())
    sem i  = let
               !z = i + 1 -- ? "avoid building up a large expression inside the IORef: 1 + 1 + 1 + ...." /Simon
             in
                (z, ())

----------------------------------------------------






-----------------------------------------------------
-----------------------------------------------------
-----------------------------------------------------
debug iter t0 sts sts' = do
    a <- getCurrentTime
    print (iter, diffUTCTime a t0, length sts, length sts')

-----------------------------------------------------
-----------------------------------------------------
ret0f t0 = do
    semaf   <- newSemaf 12
    solver t0 semaf testSt
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

