{-# LANGUAGE BangPatterns, DeriveFunctor #-}


module Rep0 where
import Sokoban

import Data.IORef (IORef (..), newIORef, readIORef, writeIORef, atomicModifyIORef, modifyIORef) 


----------------------------------------------------
----------------------------------------------------

data Arv s b = No { bit :: b
                  , val :: s 
                  , chl :: [Arv s b]
                  } deriving (Show, Functor)

aplica :: [Moves] -> MovType State
aplica m = go $ reverse m where
    go [] = Factible testSt
    go (m:ms) = 
        case go ms of
            Invalid -> Invalid
            Factible s -> funcSucess (m, s)

arvore :: Enum a => ([a] -> b) -> Arv b Bool
arvore f = No False (f []) ramos where
    ramos = map ramifica [toEnum 0 ..]
    ramifica m = arvore (f' m)
    f' m ms = f (m:ms)



indexa :: Enum c => Arv a b -> [c] -> a
indexa t []     = val t
indexa t (m:ms) = indexa (chl t !! fromEnum m) ms


indexaGet :: Enum c => Arv a b -> [c] -> b
indexaGet t []     = bit t
indexaGet t (m:ms) = indexaGet (chl t !! fromEnum m) ms



arv = arvore aplica
testeIdx = indexaGet (fmap fm arv) [Esq, Cima] 

fm a b = b








            



----------------------------------------------------
-- data MovType a = Invalid | Factible a | Goal a deriving Show
----------------------------------------------------




----------------------------------------------------
data Set a = Bin a (Set a) (Set a) |  Empty deriving (Show, Functor)
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
            
getit :: Ord a => Set a -> a -> Maybe a
getit set x = go x set where
    go x Empty = Nothing
    go x (Bin y esq dir) = 
        case compare x y of 
            LT -> go x esq
            GT -> go x dir
            _  -> Just y
            
smap :: (a -> b) -> Set a -> Set b
smap f set = go set where
    go Empty = Empty
    go (Bin x esq dir) = Bin (f x) (smap f esq) (smap f dir)

tolist :: Set a -> [a]
tolist = go where
    go Empty = []
    go (Bin a esq dir) = [a] ++ go esq ++ go dir

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




{----------------------------------------------------
atomicModifyIORef :: IORef a -> (a -> (a, b)) -> IO b
modifyIORef       :: IORef a -> (a -> a)      -> IO ()
----------------------------------------------------}


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

resetMyVar2 :: Ord a => IORef (Set a) -> IO ()
resetMyVar2 m = atomicModifyIORef m updt
  where
    updt set = (Empty, ())

resetMyVar :: Ord a => IORef (Set a) -> IO ()
resetMyVar m = modifyIORef m updt
  where
    updt set = Empty

putMyVarS :: Ord a => IORef (Set a) -> [a] -> IO ()
putMyVarS m as = atomicModifyIORef m updt
  where
    updt set = (foldl insert set as, ())



{----------------------------------------------------
popMyVar :: Ord a => IORef (Set a) -> IO [a]
popMyVar m = atomicModifyIORef m updt
  where
    updt set = (Empty, tolist set)
----------------------------------------------------}

popMyVar :: IORef (Set a) -> IO [a]
popMyVar m = atomicModifyIORef m updt
  where
    updt set = (Empty, tolist set)



lookupMyVar :: Ord t => IORef (Set t) -> t -> IO Bool
lookupMyVar m a = do
    set <- readIORef m
    return (member set a)


listMyVar :: IORef (Set a) -> IO [a]
listMyVar m = do
    set <- readIORef m
    return (tolist set)

getSetFromMyVar :: IORef (Set a) -> IO (Set a)
getSetFromMyVar m = do
    set <- readIORef m
    return set

withMyVar :: Ord a => IORef (Set a) -> a -> IO b -> IO b -> IO b
withMyVar m a act1 act2 = do
    set <- readIORef m
    if (member set a) then act1
    else do
        putMyVar m a
        act2


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
    
lengtree :: Set a -> Int
lengtree = go where
    go Empty = 0
    go (Bin _ esq dir) = 1 + go esq + go dir




















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


-- * MonadFail m
-- https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Monad-Fail.html
