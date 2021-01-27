module Main where

import Solver
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Process
---------------------------------------------------------------------------





----------------------------------------------------
main :: IO ()
main = do
    system "cls"
    t0    <- getCurrentTime
    r     <- ret0f
    t1    <- getCurrentTime
    let tempo = diffUTCTime t1 t0
    print r
    print tempo
    putStrLn $ replicate 52 '-'
    putStrLn "resposta esperada para initSt1: "
    putStrLn "[Dir,Dir,Cima,Esq,Esq,Baixo,Dir,Baixo,Dir]"
    putStrLn $ replicate 52 '-'
----------------------------------------------------

    
    
