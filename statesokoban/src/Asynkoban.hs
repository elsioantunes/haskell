{-# LANGUAGE BangPatterns #-} 
module Asynkoban where
import Sokoban
import Rep0

import System.Process (system)
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime (..))
import Control.Concurrent.Async
import Control.Exception (finally, bracket)
import Control.Monad.Par
import Control.DeepSeq (NFData, rnf)
import System.Environment
import Data.IORef (IORef (..), newIORef, readIORef, writeIORef, atomicModifyIORef, modifyIORef) 
   
-----------------------------------------------------
-----------------------------------------------------
type Jobs = [(Moves, State)]
type Box  = (Maybe State, Set State)

solver st t p1 dbug = do
    visit    <- newMyVar (eOrd st)
    stSet    <- newMyVar st
    bfs 1 visit stSet
   
  where -- escopo solver
    bfs iter visit stSet = safe iter $ do
            sts <- popMyVar stSet
            let jobs = [(m, s) | s  <- sts, m  <- actions]
            
            r <- loop jobs
            case r of
                Just x   -> return (Just x)
                Nothing-> bfs iter' visit stSet 

      where  -- escopo bfs
        iter' = iter + 1
        loop m = do
            case t of
                1 -> asyncLoop1 (particiona p1 m) 
                2 -> asyncLoop2 (particiona p1 m) 
              where
----------------------------------------------------
                asyncLoop1 :: [[(Moves, State)]] -> IO (Maybe State)
                asyncLoop1 xs = do
                    vs <- mapM (async . subloop) xs
                    rs <- mapM wait vs
                    waitloop rs

----------------------------------------------------
                asyncLoop2 :: [[(Moves, State)]] -> IO (Maybe State)
                asyncLoop2 = go where
                    go [a] = subloop a
                    go xs  = do
                        v <- async (go bs)
                        a <- go as 
                        b <- wait v
                        case (a, b) of
                            (Nothing, Nothing)  -> return Nothing
                            (Nothing, Just cm)  -> return (Just cm)
                            (Just cm, Nothing)  -> return (Just cm)
                            (Just cm, Just cm1) -> return (Just cm)
                      where
                        (as, bs) = divideBy2 xs

----------------------------------------------------
                waitloop = foldl func (return Nothing) where
                    func goxs x = do
                        case x of 
                            Nothing -> goxs
                            Just cm -> return (Just cm)

                subloop :: [(Moves, State)] -> IO (Maybe State)
                subloop = go where
                    go []     = return Nothing
                    go (x:xs) = do
                        case (funcSucess x) of
                            Goal cm     -> return (Just cm)
                            Invalid     -> go xs
                            Factible cm -> do

                                look <- lookupMyVar visit (eOrd cm)
                                if look then 
                                    go xs
                                    
                                else do
                                    putMyVar visit (eOrd cm)
                                    putMyVar stSet cm
                                    go xs

iterlimit = 50

-----------------------------------------------------
actions :: [Moves]
actions = [toEnum 0 ..]
    
walk :: [State] -> Jobs
walk sts = [(m, s) | s  <- sts, m  <- actions]
-----------------------------------------------------
main :: IO ()
main = do
        system "clear"
        t0 <- getCurrentTime
        r <- ret0f t0
        print r
        t1 <- getCurrentTime
        print (diffUTCTime t1 t0)

ret0f :: UTCTime -> IO (Maybe State)
ret0f t0 = do
    solver testSt 1 100 True

taskIO t p1 = do
    t0 <- getCurrentTime
    solver testSt t p1 False
    t1 <- getCurrentTime
    print (t, p1, diffUTCTime t1 t0)

{----------------------------------------------------
----------------------------------------------------}

particiona :: Int -> [a] -> [[a]]
particiona n m = go m n where
    go _ 0 = []
    go m n = a : go b (n-1) where
        (a, b) = splitAt (length m `div` n) m
 
particionaThresh :: Int -> [a] ->  [[a]]
particionaThresh n = go where
    go [] = []
    go xs = f $ splitAt n xs
        where 
            f (as, bs) = (as : go bs)

divideBy :: Int -> [a] -> ([a], [a])
divideBy n m = splitAt (length m `div` n)  m
divideBy2 m  = divideBy 2 m

safe :: Monad m => Int -> m (Maybe a) -> m (Maybe a)
safe iter io | iter > iterlimit = return Nothing 
             | otherwise = io


when :: Applicative f => Bool -> f () -> f ()
when p s  | p = s 
          | otherwise = pure ()

debug dbug iter t0 sts sts' = do
    if not dbug then do
        return ()
    else do
        a <- getCurrentTime
        print (iter, diffUTCTime a t0, length sts, length sts')


{----------------------------------------------------
cont1 []     = 0
cont1 (x:xs) | push x = (cont1 xs) + 1
             | otherwise = cont1 xs

graph iter sts = go c1 c2 where
    c1 = cont1 sts
    c2 = length sts 
    x  = round (80*(fromIntegral c1)/(fromIntegral c2))
    go a b = do 
        putStrLn ((replicate x '#') ++ (replicate (80-x) '.') ++ " " ++ show iter)
        

rip :: [State] -> [State]
rip m = r ++ l where
    (l, r) = foldr func ([], []) m
    func x (a, b) | push x = (x:a, b)
                  | otherwise = (a, x:b)

filterVisit :: (a -> IO Bool) -> [a] -> IO [a]
filterVisit pred = go where

    go [] = return []
    go (x:xs) = do
        f  <- ok pred x
        ks <- go xs
        return (f ks)

    
    ok :: (a -> IO Bool) -> a -> IO ([a] ->  [a])
    ok pred x = do
        r <- pred x 
        case r of
            True -> return (x:)
            _    -> return id
    

----------------------------------------------------}

-----------------------------------------------------
-- componente conexa



    
    
    
    