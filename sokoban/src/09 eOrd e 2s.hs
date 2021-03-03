{-# LANGUAGE BangPatterns #-}
module Main where

import Sokoban
import Rep0

import System.Process (system)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Control.Concurrent (MVar (..), putMVar, takeMVar, newMVar)
import Control.Concurrent.Async
import Data.IORef (IORef (..), newIORef, readIORef, atomicModifyIORef) 
import Control.Exception (finally, bracket)

-----------------------------------------------------
-----------------------------------------------------
solver t0 semaf st = do
    set <- newMyVar (eOrd st)
    bfs 1 set [st]
   
   where 
    bfs iter set sts  = do
    
        -- sts <- listMyVar set
        
        let ts= [(m, s) | s  <- sts, m  <- actions]
        
        debug iter t0 sts ts
        
        r <- loopIO set ts
        -- r <- subloop [] sts' 
            
        case r of
            (Nothing, sts') -> bfs iter' set sts'
            (Just x, _)     -> return (Just x)

      where    
        iter' = iter + 1
        
{----------------------------------------------------
        subloop asyncs [] = loopIO set asyncs
        subloop asyncs (x:xs) = do

                q <- obtemTicket semaf

                if q then do
                    let dofind = (uncurry funcSucess x) `finally` (liberaSemaf semaf)
                    withAsync dofind $ \a -> 
                        subloop (a:asyncs) xs

                else do 
                    r <- uncurry funcSucess x
                    case r of  
                        Goal cm     -> return (Just cm)
                        Invalid     -> subloop asyncs xs
                        Factible cm -> do
                            r <- lookupMyVar set cm
                            if r then 
                                subloop asyncs xs

                            else do
                                putMyVar set cm
                                subloop asyncs xs
----------------------------------------------------}



    loopIO set ts = go ts [] where
        go []     = \acc -> return (Nothing, acc)
        go (x:xs) = \acc -> do

            r <- uncurry funcSucess x
            -- r <- wait x

            case r of
                Goal cm     -> return ((Just cm), [])
                Invalid     -> go xs acc
                Factible cm -> do
                    r <- lookupMyVar set (eOrd cm)
                    if r then 
                        go xs acc

                    else do
                        putMyVar set (eOrd cm)
                        go xs (cm:acc)



actions :: [Moves]
actions = [toEnum 0 ..]
    













----------------------------------------------------
-- MyVar IORef -------------------------------------
-- newMyVar :: Ord a => a -> IO (IORef (Set a))
newMyVar a = do
    m <- newIORef (insert Empty a)
    return m

-- putMyVar :: Ord a => IORef (Set a) -> a -> IO ()
putMyVar m a = atomicModifyIORef m updt
  where
    updt set = (insert set a, ())

-- lookupMyVar :: Ord t => IORef (Set t) -> t -> IO Bool
lookupMyVar m a = do
    set <- readIORef m
    return (member set a)
    
    
-- listMyVar :: IORef (Set a) -> IO [a]
listMyVar m = do
    set <- readIORef m
    return (tolist set)

----------------------------------------------------
----------------------------------------------------




{----------------------------------------------------
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
----------------------------------------------------}
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
    semaf   <- newSemaf 3
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

