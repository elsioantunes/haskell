{-# LANGUAGE BangPatterns #-}
module Rep0 where
import Sokoban

import Data.IORef (IORef (..), newIORef, readIORef, writeIORef, atomicModifyIORef) 

{----------------------------------------------------
----------------------------------------------------
arv = arvore aplica
testeIdx = indexa arv [Cima]


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
----------------------------------------------------}






----------------------------------------------------
-- data MovType a = Invalid | Factible a | Goal a deriving Show
----------------------------------------------------




----------------------------------------------------
data Set a = Bin a (Set a) (Set a) |  Empty deriving Show
data Arv s = No s [Arv s] deriving Show
----------------------------------------------------
insert :: Ord a => Set a -> a -> Set a
insert set x = go x set where
    go x Empty = Bin x Empty Empty
    go x t@(Bin y esq dir) = 
        case compare x y of 
            LT -> Bin y (go x esq) dir
            GT -> Bin y esq (go x dir)
            _  -> t

member :: Ord t => Set t -> t -> Bool
member set x = go x set where
    go x Empty = False
    go x (Bin y esq dir) = 
        case compare x y of 
            LT -> go x esq
            GT -> go x dir
            _  -> True
            
tolist :: Set a -> [a]
tolist = go where
    go Empty = []
    go (Bin a esq dir) = go esq ++ [a] ++ go dir

setNew :: Ord a => a -> Set a
setNew x = do
    insert Empty x

merge Empty t2 = t2
merge (Bin a esq dir) t2 = Bin a (merge esq t2) dir

fromList :: (Foldable t, Ord a) => Set a -> t a -> Set a
fromList set m = foldl insert set m

map1 f = go where
    go Empty = Empty
    go (Bin a esq dir) = Bin (f a) (go esq) (go dir)



----------------------------------------------------
----------------------------------------------------








----------------------------------------------------
-- MyVar IORef -------------------------------------
newMyVar :: Ord a => a -> IO (IORef (Set a))
newMyVar a = do
    m <- newIORef (insert Empty a)
    return m

putMyVarS :: Ord a => IORef (Set a) -> [a] -> IO ()
putMyVarS m as = atomicModifyIORef m updt
  where
    updt set = (foldl insert set as, ())

listMyVar :: IORef (Set a) -> IO [a]
listMyVar m = do
    set <- readIORef m
    return (tolist set)

withMyVar :: Ord a => IORef (Set a) -> a -> IO b -> IO b -> IO b
withMyVar m a act1 act2 = do
    set <- readIORef m
    if (member set a) then act1
    else do
        putMyVar m a
        act2

putMyVar :: Ord a => IORef (Set a) -> a -> IO ()
putMyVar m a = atomicModifyIORef m updt
  where
    updt set = (insert set a, ())

lookupMyVar :: Ord t => IORef (Set t) -> t -> IO Bool
lookupMyVar m a = do
    set <- readIORef m
    return (member set a)


filterMyVarsWith :: (Ord a, Ord b) => IORef (Set b) -> (a -> b) -> Set a -> IO (Set a)
filterMyVarsWith m eOrd as = atomicModifyIORef m updt
  where
    updt bs = (bs, cs)
      where
        cs = filtreeSet bs eOrd as
    

{----------------------------------------------------
----------------------------------------------------}

filtree :: Ord a => (a -> Bool) -> Set a -> Set a
filtree p set = foldr ins Empty (tolist set) 
  where
    ins x goxs | p x =  insert goxs x
               | otherwise = goxs


filtreeSet :: (Ord a, Ord b) => Set b -> (a -> b) -> Set a -> Set a
filtreeSet bs eOrd as = filtree (not . member bs . eOrd) as
    

lengtree = go where
    go Empty = 0
    go (Bin _ esq dir) = 1 + go esq + go dir






















-- * MonadFail m
-- https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Monad-Fail.html
