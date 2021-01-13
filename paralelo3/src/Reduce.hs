module Reduce where

import Control.Monad.Par
import Control.DeepSeq (NFData, rnf)
import Data.Time.Clock
import Task

data IList a = Nil | Cons a (IVar (IList a)) | Fork (Par ()) (IList a)

instance NFData a => NFData (IList a) where
  rnf Nil = ()
  rnf (Cons a b) = rnf a `seq` rnf b
  rnf (Fork a b) = a `seq` rnf b
-----------------------------------------------------
-----------------------------------------------------




-----------------------------------------------------
-----------------------------------------------------
reduce0 :: [Integer] -> Par Int
reduce0 = go where
    go [a] = return (resolve a)
    go xs = do
        let (as, bs) = divideBy2 xs
        v <- spawn (go bs)
        a <- go as
        b <- get v
        return (junta a b)
-----------------------------------------------------
-----------------------------------------------------
        



-----------------------------------------------------
-----------------------------------------------------
reduce1 :: [Integer] -> Par Int
reduce1 = go  where
    go [a] = return (resolve a)
    
    go xs | length xs < 50 = do
                a <- go as
                b <- go bs
                return (junta a b)
          
          | otherwise = do
                v <- spawn (go bs)
                a <- go as
                b <- get v
                return (junta a b)
      where
        (as, bs) = divideBy2 xs
-----------------------------------------------------
-----------------------------------------------------
        





-----------------------------------------------------
-----------------------------------------------------
reduce1a :: [Integer] -> Par Int
reduce1a = go  where
    
    go xs | length xs < 50 = return (some resolve junta xs)
          | otherwise = do
                v <- spawn (go bs)
                a <- go as
                b <- get v
                return (junta a b)
      where
        (as, bs) = divideBy2 xs


some :: (a -> b) -> (b -> b -> b) -> [a] -> b
some f g [x] = f x
some f g (x:xs) = (g . f) x (some f g xs)

-----------------------------------------------------
-----------------------------------------------------
        
        
        
        



-----------------------------------------------------
-----------------------------------------------------
reduce2 :: [Integer] -> Par Int
reduce2 = go . chunks 440 where
    
    go [xs] = return (some resolve junta xs)
    go xss  = do
                v <- spawn (go bs)
                a <- go as
                b <- get v
                return (junta a b)
      where
        (as, bs) = divideBy2 xss

-----------------------------------------------------
chunks = go  where
    go _ [] = []
    go 1 xs = [xs]
    go n xs = f $ divideBy n xs
        where 
            f (as, bs) = as : go (n-1) bs

-- chunks 3 [5..19]  =  
-- [[5,6,7,8,9],[10,11,12,13,14],[15,16,17,18,19]]

-----------------------------------------------------
-----------------------------------------------------
        







-----------------------------------------------------
-----------------------------------------------------
reduce3 :: [Integer] -> Par Int
reduce3 = go . chunks 440 where
    
    -- go [xs] = return (some resolve junta xs)
    
    bo xs | length xs < 5 = return (some resolve junta xs)
          | otherwise = do
                v <- spawn (bo cs)
                a <- bo ds
                b <- get v
                return (junta a b)
      where
        (cs, ds) = divideBy2 xs
                
    go [xs] = bo xs

    go xss  = do
                v <- spawn (go bs)
                a <- go as
                b <- get v
                return (junta a b)
      where
        (as, bs) = divideBy2 xss
    
    some :: (a -> b) -> (b -> b -> b) -> [a] -> b
    some f g = go where
        go [x] = f x
        go (x:xs) = (g . f) x (go xs)


-----------------------------------------------------
-----------------------------------------------------
        









-----------------------------------------------------
-----------------------------------------------------
main :: IO ()
main = do
    putStrLn "...."
    
{-  reduce 0
    apesar de já ganhar do pipe3(200) em tempo de execução
    tem bytes alocados na Heap maiores que todos os pipes
    além, é claro, de GC e residency altas
    (com MUT baixo)
    tamanho do arquivo eventlog baixo
    comparável ao pipe1
    
    print $ runPar $ reduce0 [1..10000000]
    
-}

{-  reduce1 - inserção de threshold
    diminuição do uso de heap , de 38GB para 24GB
    (mas ainda longe dos 16GB do pipe)
    
    5GB de CG
    também nem perto dos 66MB do pipe
    residency e slop tb altos
    
    queda de tempo em relação ao reduce0
    provavelmente puchada pelo MUT
    
    arquivo eventlog pequeno.
    o menor até agora. (reduce2 venceu)
    
    print $ runPar $ reduce1 [1..10000000]
    
    --------
    
    versão 1a com ganho de heap de 24 para 19
    (talvez entrar e sair do monad gaste heap demais)
    
    print $ runPar $ reduce1a [1..10000000]
-}



{-  reduce2, range (change the granularity)
    uso da heap caiu pela metade (menor até agora),  mas ...
    trabalho do GC dobrou, MUT aumentou 
    tempo aumentou em um terço! 
    (arquivo eventlog pequeno, metade do reduce1)
    
    [BOA PARTE DO TRABALHO FOI FEITO EM SERIAL]
    segundo threadscope
    
    print $ runPar $ reduce2 [1..10000000]
    
-}

    print $ runPar $ reduce3 [1..10000000]





-----------------------------------------------------
-- Bench --------------------------------------------
params :: [Int]
params = [func i | i <- [16..21] ]
func i = i*i


taskIO bla cmd = do 
           t0 <- getCurrentTime
           print bla
           print $ runPar $ cmd
           t1 <- getCurrentTime
           print (diffUTCTime t1 t0)
           putStrLn "---------------------"

-- utils --------------------------------------------
divideBy :: Int -> [a] -> ([a], [a])
divideBy n m = splitAt (length m `div` n)  m
divideBy2 m = divideBy 2 m


{--- Nota --------------------------------------------
    stack exec reduce --RTS -- +RTS -N -s -ls






-}----------------------------------------------------



