module Main where
import Data.Time.Clock
import Fibo
import Solver
------------------------------ ------------------------------
main = do 
        t0 <- getCurrentTime
        putStrLn "--------------------------------------"
        ret
        putStrLn "--------------------------------------"
        t1 <- getCurrentTime
        print (diffUTCTime t1 t0)

        




