{-# LANGUAGE BangPatterns, DeriveFunctor #-}


module Rep0 where
import Sokoban

import Data.IORef (IORef (..), newIORef, readIORef, writeIORef, atomicModifyIORef, atomicModifyIORef', modifyIORef) 
import Control.Concurrent (putMVar, takeMVar, newEmptyMVar, newMVar, modifyMVar, MVar(..))




----------------------------------------------------
data Set a = Bin a (Set a) (Set a) |  Empty deriving (Show, Functor)
----------------------------------------------------
setNew :: Ord a => a -> Set a
setNew x = do
    insert Empty x

insert :: Ord a => Set a -> a -> Set a
insert set x = go x set where
    go x Empty = Bin x Empty Empty
    go x t@(Bin y esq dir) = 
        case compare x y of 
            LT -> Bin y (go x esq) dir
            GT -> Bin y esq (go x dir)
            _  -> t

empty :: Set a -> Bool
empty Empty = True
empty _ = False
            
member :: Ord a => Set a -> a -> Bool
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
    go (Bin a esq dir) = [a] ++ go esq ++ go dir

merge Empty t2 = t2
merge (Bin a esq dir) t2 = Bin a (merge esq t2) dir

fromList :: (Foldable t, Ord a) => Set a -> t a -> Set a
fromList set m = foldl insert set m

----------------------------------------------------
----------------------------------------------------



----------------------------------------------------
-- MyVar IORef -------------------------------------
type MyVar a = IORef (Set a)

newMyVar :: Ord a => a -> IO (MyVar a)
newMyVar a = do
    m <- newIORef (insert Empty a)
    return m

putMyVar :: Ord a => MyVar a -> a -> IO ()
putMyVar m a = atomicModifyIORef' m updt 
  where
    updt set = (insert set a, ())

popMyVar :: MyVar a -> IO [a]
popMyVar m = atomicModifyIORef' m updt
  where
    updt set = (Empty, tolist set)

lookupMyVar :: Ord a => MyVar a -> a -> IO Bool
lookupMyVar m a = do
    set <- readIORef m
    return (member set a)



-- sucata 
newEmptyMyVar :: Ord a => IO (MyVar a)
newEmptyMyVar = do
    m <- newIORef (Empty)
    return m

notInMyVarIO :: Ord a => IORef (Set a) -> a -> IO () -> IO ()
notInMyVarIO m a io = do
    set <- readIORef m
    case (member set a) of
        True -> pure ()
        _ -> do
            writeIORef  m (insert set a)
            io

notInMyVar :: Ord a => MyVar a -> a -> IO Bool
notInMyVar m a = do
    set <- readIORef m
    return (not $ member set a)

listMyVar :: MyVar a -> IO [a]
listMyVar m = do
    set <- readIORef m
    return (tolist set)

resetMyVar :: Ord a => IORef (Set a) -> IO ()
resetMyVar m = modifyIORef m updt
  where
    updt set = Empty
    
    
    
    
    

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

    