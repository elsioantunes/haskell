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

-----------------------------------------------------
-----------------------------------------------------
type Jobs = [(Moves, State)]
type Box  = (Maybe State, Set State)


solver t t0 st p2 p3 dbug = do
    visit    <- newMyVar (eOrd st)
    stSet    <- newMyVar st
    bfs 1 visit stSet
   
  where -- escopo solver
    bfs iter visit stSet  = do

        if iter > 150 then do return Nothing 
        else do 
            sts <- popMyVar stSet
            let jobs = [(m, s) | s  <- sts, m  <- actions]

            debug dbug iter t0 jobs sts

            
            r <- loop jobs
            case r of
                Just x   -> return (Just x)
                Nothing-> bfs iter' visit stSet 

      where  -- escopo bfs ---------------------------------
        ----------------------------------------------------
        iter' = iter + 1
        ----------------------------------------------------
        loop :: Jobs -> IO (Maybe State)
        loop m = 
            driver (particiona p2 m) 

              where
                driver :: [Jobs] -> IO (Maybe State)
                driver = go  where
                    go []       = return Nothing
                    go (xs:xss) = do
                        r <- subloop xs
                        case r of                                                   
                            Just cm -> return r
                            Nothing -> 
                                go xss

                                                                     
                subloop :: Jobs -> IO (Maybe State)
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
                                

ret0f :: UTCTime -> IO (Maybe State)
ret0f t0 = do
    [args] <- getArgs
    solver 1 t0 testSt 1334 (read args :: Int) True

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

taskIO t p2 = do
    t0 <- getCurrentTime
    solver t t0 testSt p2 666 False
    t1 <- getCurrentTime
    print (diffUTCTime t1 t0)
-----------------------------------------------------