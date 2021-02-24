{-# LANGUAGE BangPatterns #-}
module Main where
import System.Process (system)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
-- import Control.Concurrent
import Sokoban

-- import Control.Concurrent.Async
import Data.IORef (IORef (..), atomicModifyIORef, newIORef)
import Control.Exception (finally)                


-----------------------------------------------------
-----------------------------------------------------
-- solver t0 semaf st = bfs 1 (setNew $ ordSt st) (setNew $ ordSt st)
solver t0 semaf st = bfs 1 (setNew $ ordSt st)
  where 
    -- bfs :: Int -> Set A -> Set A -> IO (Maybe A)
    -- bfs iter set localSet = do
    bfs iter set = do
    
        debug iter t0 []
        
        -- r <- loopIO set [(m, s) | s  <- tolist localSet, m  <- actions]
        r <- loopIO set [(m, s) | s  <- tolist set, m  <- actions]
            
        case r of
            -- (Nothing, sts') -> bfs iter' (updateSet set sts') (updateSet Empty sts')
            (Nothing, sts') -> bfs iter' (updateSet set sts')
            (Just x, _)     -> return (Just x)

      where    
        iter' = iter + 1
        
        actions :: [Moves]
        actions = [toEnum 0 ..]

loopIO :: Set A -> [(Moves, A)] -> IO (Maybe A, [A])
loopIO set as = return ((go as) []) 
  
  where
    go [] = (\acc ->  (Nothing, acc))
    go (x:xs) = func x (go xs)
    
    func x goxs = \acc ->  
        case uncurry funcSucess x of
            Nothing -> goxs acc 
            Just cm -> 
                if (member set cm) then
                    goxs acc 
                    
                else
                    if isgoal cm then do
                        (Just cm, [])
                    else
                        goxs (cm:acc)
    




-- 2.825007387s


{----------------------------------------------------
-- guardando set em Mvars's ---------------------

newLevelM :: a -> IO (MVar a)
newLevelM xs = do
    v <- newEmptyMVar
    forkIO (putMVar v xs)
    return v

insLevelM :: MVar [a] -> a -> IO ()
insLevelM v x = modifyMVar v lvl
  where
    lvl xs = return $ let 
                !z = x:xs
             in 
                (z, ())

getLevelsM :: MVar [a] -> IO [a]
getLevelsM v = modifyMVar v lvl
  where
    lvl xs = return $ ([], xs)
----------------------------------------------------}

-----------------------------------------------------
{-----------------------------------------------------
newSete x = do
    v <- newMVar x
    return v

getSete v = do
    sete <- takeMVar v
    putMVar v sete
    return sete
    
updtSete v newv = do
    sete <- takeMVar v
    putMVar v newv
    return ()
    
    


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






{----------------------------------------------------
teste3 = do
    let a = insert  Empty 60
        b = insert  a 47
        c = insert  b 47
        d = insert  c 666
    print (map (member d) [60,47,66,666,7] )
----------------------------------------------------}













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
debug iter t0 sts' = do
    a <- getCurrentTime
    print (iter, diffUTCTime a t0, length sts')

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



--------------------------
taskIO n = do
    t0     <- getCurrentTime
    semaf  <- newSemaf n
    solver t0 semaf testSt
--------------------------





bla = [
    (1,0,2,4,1),
    (2,0,4,8,2),
    (3,0,3,16,4),
    (4,0,10,12,3),
    (5,0,24,40,10),
    (6,0,51,96,24),
    (7,0,86,204,51),
    (8,0,168,344,86),
    (9,0,266,672,168),
    (10,0,435,1064,266),
    (11,0,638,1740,435),
    (12,0,1027,2552,638),
    (13,0,1474,4108,1027),
    (14,0.0156257,2282,5896,1474),
    (15,0.0312523,3182,9128,2282),
    (16,0.0522873,4672,12728,3182),
    (17,0.077765,6453,18688,4672),
    (18,0.1246415,9714,25812,6453),
    (19,0.1833219,13221,38856,9714),
    (20,0.2995227,19399,52884,13221),
    (21,0.4401559,26481,77596,19399),
    (22,0.6589219,38561,105924,26481),
    (23,1.0026936,51331,154244,38561),
    (24,1.4558493,73987,205324,51331),
    (25,2.1277683,97925,295948,73987),
    (26,3.1122077,137080,391700,97925),
    (27,4.6000074,176896,548320,137080),
    (28,6.6782705,243552,707584,176896),
    (29,9.4284502,303644,974208,243552),
    (30,13.3428521,411859,1214576,303644),
    (31,18.5931982,511039,1647436,411859),
    (32,25.5964086,693431,2044156,511039),
    (33,35.0973047,841380,2773724,693431),
    (34,47.4913502,1123761,3365520,841380)]
    
{----------------------------------------------------
xablau = [] where
    blau [] = []
    blau ((a, b, c, d, e):xs) = b : blau xs
----------------------------------------------------}


-- [2,4,3,10,24,51,86,168,266,435,638,1027,1474,2282,3182,4672,6453,9714,13221,19399,26481,38561,51331,73987,97925,137080,176896,243552,303644,411859,511039,693431,841380,1123761]
-- [4,8,16,12,40,96,204,344,672,1064,1740,2552,4108,5896,9128,12728,18688,25812,38856,52884,77596,105924,154244,205324,295948,391700,548320,707584,974208,1214576,1647436,2044156,2773724,3365520]
-- [1,2,4,3,10,24,51,86,168,266,435,638,1027,1474,2282,3182,4672,6453,9714,13221,19399,26481,38561,51331,73987,97925,137080,176896,243552,303644,411859,511039,693431,841380]
-- [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.56257e-2,3.12523e-2,5.22873e-2,7.7765e-2,0.1246415,0.1833219,0.2995227,0.4401559,0.6589219,1.0026936,1.4558493,2.1277683,3.1122077,4.6000074,6.6782705,9.4284502,13.3428521,18.5931982,25.5964086,35.0973047,47.4913502]


{-
    hill climbing search?
    https://towardsdatascience.com/solve-slide-puzzle-with-hill-climbing-search-algorithm-d7fb93321325
    
-}