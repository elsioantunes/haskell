module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime (..))
import System.Process (system)

import Sokoban
import State



----------------------------------------------------
----------------------------------------------------
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
    solver testSt True
----------------------------------------------------
