{-# LANGUAGE BangPatterns #-}
module Rep0 where
import Sokoban

import Data.IORef (IORef (..), newIORef, readIORef, atomicModifyIORef) 

arv = arvore aplica
testeIdx = indexa arv [Cima]


----------------------------------------------------
----------------------------------------------------
aplica :: [Moves] -> IO (MovType State) 
aplica []     = return (Factible testSt)
aplica (m:ms) = do
    (Factible s) <- aplica ms -- * MonadFail
    funcSucess m s

----------------------------------------------------
arvore :: Enum mv => ([mv] -> st) -> Arv st
arvore f = No (f []) ramos where
    ramos = map ramifica [toEnum 0 ..]
    ramifica m = arvore (f' m)
    f' m ms = f (m:ms)

indexa :: Enum mv => Arv st -> [mv] -> st
indexa (No x ts) []     = x
indexa (No x ts) (m:ms) = indexa (ts !! fromEnum m) ms

----------------------------------------------------
----------------------------------------------------




----------------------------------------------------
data Set a = Bin a (Set a) (Set a) |  Empty deriving Show
data Arv s = No s [Arv s] deriving Show
----------------------------------------------------
insert set x = go x set where
    go x Empty = Bin x Empty Empty
    go x t@(Bin y esq dir) = 
        case compare x y of 
            LT -> Bin y (go x esq) dir
            GT -> Bin y esq (go x dir)
            _  -> t


eOrd st = (player st, calc (box st))

setNew x = do
    insert Empty x

tolist = go where
    go Empty = []
    go (Bin a esq dir) = go esq ++ go dir ++ [a]


updateSet set m = foldl insert set m

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
-- MyVar IORef -------------------------------------
newMyVar :: Ord a => a -> IO (IORef (Set a))
newMyVar a = do
    m <- newIORef (insert Empty a)
    return m

putMyVar :: Ord a => IORef (Set a) -> a -> IO ()
putMyVar m a = atomicModifyIORef m updt
  where
    updt set = (insert set a, ())

lookupMyVar :: Ord t => IORef (Set t) -> t -> IO Bool
lookupMyVar m a = do
    set <- readIORef m
    return (member set a)
    
listMyVar :: IORef (Set a) -> IO [a]
listMyVar m = do
    set <- readIORef m
    return (tolist set)

----------------------------------------------------
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
----------------------------------------------------






{----------------------------------------------------
----------------------------------------------------
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
insert set x = go x set where
    go x Empty = Bin x Empty Empty
    go x t@(Bin y esq dir) = 
        case compare x y of 
            LT -> Bin y (go x esq) dir
            GT -> Bin y esq (go x dir)
            _  -> t

member set x = go x set where
    go x Empty = False
    go x (Bin y esq dir) = 
        case compare x y of 
            LT -> go x esq
            GT -> go x dir
            _  -> True
----------------------------------------------------}













-- * MonadFail m
-- https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Monad-Fail.html
