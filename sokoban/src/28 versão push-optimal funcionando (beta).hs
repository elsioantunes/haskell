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

solver st dbug = do
    explr  <- newMyVar (eOrd st)
    nivls  <- newMyVar st
    ret    <- newMyVar st
    t0     <- getCurrentTime
    bfs 1 nivls explr ret t0

  where
    bfs iter nivls explr ret t0 = safe $ do
        r <- mainloop
        case r of 
            Just x  -> return (Just x)
            Nothing -> bfs (iter + 1) nivls explr ret t0

          where
            mainloop :: IO (Maybe State)
            mainloop = do
                sts <- popMyVar nivls
                
                if not $ null sts then do 
                    go (walk sts)
                    
                else do
                    sts <- popMyVar ret
                    go (walk sts)
            
                  where
                    alk sts = [(m, s) | s  <- sts, m  <- actions]
                    
                    go :: Jobs -> IO (Maybe State)
                    go [] = return Nothing
                    go (x:xs) = do
                        case (funcSucess x) of
                            Goal cm     -> return (Just cm)
                            Invalid     -> go xs
                            Factible cm -> do
                                look <- lookupMyVar explr (eOrd cm)
                                if look then
                                    go xs
                                else do
                                    putMyVar explr (eOrd cm)
                                    putMyVar nivls cm
                                    if not $ push cm then
                                        go xs
                                    else do
                                        putMyVar ret cm
                                        go xs

            safe io = if iter > 100 then return Nothing else io

actions :: [Moves]
actions = [toEnum 0 ..]
    
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

debug dbug iter t0 sts sts' = do
    if not dbug then do
        return ()
    else do
        a <- getCurrentTime
        print (iter, diffUTCTime a t0, length sts, length sts')

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
    [args] <- getArgs
    -- solver 1 t0 testSt 1334 (read args :: Int) True
    solver testSt True

taskIO t p2 = do
    t0 <- getCurrentTime
    solver testSt False
    t1 <- getCurrentTime
    print (diffUTCTime t1 t0)
-----------------------------------------------------