module Fibo where
import Control.Monad.Par

-------------------------------------------------------------
testeFibo :: Integer
testeFibo = runPar $ fibo 30

fibo :: Integer -> Par Integer
fibo n
      | n < 2 = return 1
      | otherwise = do
                      mm <- spawn(fibo (n-1))
                      a <- fibo (n-2)
                      b <- get mm
                      return (a + b)
-------------------------------------------------------------





                      

-----------------------------------------------------
-----------------------------------------------------
lista :: [Double]
lista = [1 .. 10000000]
testeReduce g = runPar (g (+) lista)
-----------------------------------------------------
ret = do 
        print  testeFibo
        -- print $ testeReduce reduce1
        -- print $ virgula2 100000
        -- print $ virgula 100000
        print $ testeReduce reduce
-----------------------------------------------------



               
-----------------------------------------------------
reduce :: NFData a => (a -> a -> a) -> [a] -> Par a
reduce f = go where
    go [a] = return a
    go xs = do
               mm <- spawn $ (go bs)
               x <- go as
               y <- get mm 

               return (f x y)
               
        where               
            (as, bs) = splitAt(length xs `div` 2) xs
                
-----------------------------------------------------





{-----------------------------------------------------
spawn' :: NFData a => Par a -> Par (IVar a)
spawn' p = do
  v <- new
  -- fork $ p >>= put v
  fork $ do 
           r <- p
           put v r
  return v
-----------------------------------------------------}