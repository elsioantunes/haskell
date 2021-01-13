module Pipe where

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
pipe0 ::[Integer] -> Par (Int)
pipe0 xs = do 
               -- crio n IVars
              vs <- replicateM new xs
              
              -- aplico f em cada x e crio threads para forcar avaliacao
              mapM_ (forkWith resolve) (zip xs vs) 
              
              -- retiro os valores avaliados das IVars 
              ys <- mapM get vs
              
              -- retorno a combinacao entre eles num valor unico
              return (foldr junta 0 ys)

-----------------------------------------------------
forkWith :: NFData a => (b -> a) -> (b, IVar a) -> Par ()
forkWith f (x, v) = fork $ put v (f x)
-----------------------------------------------------
-----------------------------------------------------






-----------------------------------------------------
-----------------------------------------------------
-- [Integer] -> Par [IVar Int]  >>=   [IVar Int] -> Par Int
pipe0a :: Junta Int -> Resolve Integer Int -> [Integer] -> Par (Int)
pipe0a j f xs = mapForkWith xs >>= mapFold
-----------------------------------------------------
recursivo :: Monad m => (a -> m b) -> (b -> c -> c) -> c -> [a] -> m c
recursivo f g x0 = go where
    go [] = return x0
    go (x:xs) = do 
                  v <- f x
                  vs <- go xs
                  return (g v vs)

-----------------------------------------------------
mapForkWith :: [Integer] -> Par [IVar Int]
mapForkWith = recursivo (spawnP . resolve) (:) []

mapFold :: [IVar Int] -> Par Int
mapFold = recursivo get junta 0
-----------------------------------------------------
-----------------------------------------------------







-----------------------------------------------------
-----------------------------------------------------
pipe1 :: [Integer] -> Par Int
pipe1 xs = sfold 0 =<< toilist xs
-----------------------------------------------------
toilist m = do
    v <- new
    fork $ loop m v
    return v

  where
    loop [] v     = put v Nil
    loop (x:xs) v = do
                     nv <- new
                     put v $ Cons x nv
                     loop xs nv
-----------------------------------------------------
sfold :: Int -> IVar (IList Integer) -> Par Int
sfold acc m = acc `seq` do
    list <- get m
    case list of 
        Nil -> return acc
        Cons x xs -> sfold (junta acc (resolve x)) xs
-----------------------------------------------------
-----------------------------------------------------




-----------------------------------------------------
-----------------------------------------------------
pipe1a :: [Integer] -> Par Int
pipe1a xs = sfold 0 =<< toilist xs
-----------------------------------------------------
  where
    toilist m = do
        v <- new
        fork $ loop m v
        return v

      where
        loop [] v     = put v Nil
        loop (x:xs) v = do
                         nv <- new
                         put v $ Cons (resolve x) nv
                         loop xs nv
-----------------------------------------------------
    sfold acc m = acc `seq` do
        list <- get m
        case list of 
            Nil -> return acc
            Cons x xs -> sfold (junta acc x) xs
-----------------------------------------------------
-----------------------------------------------------




-----------------------------------------------------
-----------------------------------------------------
pipe2 :: [Integer] -> Par Int
pipe2 xs = sfold2 0 =<< smap =<< toilist xs
-----------------------------------------------------
smap :: IVar (IList Integer) -> Par (IVar (IList Int))
smap m = do
    v <- new
    fork $ loop m v
    return v
  where
    loop m v = do
        list <- get m
        case list of 
            Nil       -> put v Nil
            Cons x xs -> consFx v x xs 
    
    consFx v x xs = do
                      nv <- new
                      put v $ Cons (resolve x) nv
                      loop xs nv

-----------------------------------------------------
sfold2 :: Int -> IVar (IList Int) -> Par Int
sfold2 acc m = acc `seq` do
    list <- get m
    case list of 
        Nil -> return acc
        Cons x xs -> sfold2 (junta acc x) xs
-----------------------------------------------------
-----------------------------------------------------
















-----------------------------------------------------
-----------------------------------------------------
pipe3 :: Int -> [Integer] -> Par Int
pipe3 n xs = sfold3 0 =<< smap3 =<< toilist3 n xs
-----------------------------------------------------
toilist3 n m = do
    v <- new
    fork $ loop n m v
    return v
  where
    loop _ [] v     = put v Nil
    
    loop 0 (x:xs) v = do
        nv <- new
        put v $ Fork (loop n xs nv) (Cons x nv)
                     
    loop n (x:xs) v = do
        nv <- new
        put v $ Cons x nv
        loop (n-1) xs nv
-----------------------------------------------------
smap3 m = do
    v <- new
    fork $ loop m v
    return v
  where
    loop m v = do
        list <- get m
        case list of 
            Nil                 -> put v Nil
            Cons x xs           -> consFx v x xs 
            Fork op (Cons x xs) -> fork op >> consFx v x xs 
    consFx v x xs = do
                      nv <- new
                      put v $ Cons (resolve x) nv
                      loop xs nv
-----------------------------------------------------
sfold3 :: Int -> IVar (IList Int) -> Par Int
sfold3 acc m = acc `seq` do
-- sfold3 acc m = do
    list <- get m
    case list of 
        Nil                  -> return acc
        Cons x xs            -> sfold3 (junta acc x) xs
        Fork op (Cons x xs)  -> fork op >> sfold3 (junta acc x) xs
-----------------------------------------------------
-----------------------------------------------------





















-----------------------------------------------------
chunk_iList :: NFData a => IVar (IList a) -> Int -> Par ([IVar (IList a)])
chunk_iList m 1 = return [m]
chunk_iList m n = do
                   len <- len_iList m
                   (as, bs) <- splitAt_iList m (div len n) 
                   bss <- chunk_iList bs (n-1)
                   return (as:bss)

-----------------------------------------------------
splitAt_iList :: NFData a =>
     IVar (IList a) -> Int -> Par (IVar (IList a), IVar (IList a))
splitAt_iList m n = do
    v1 <- take_iList m n
    v2 <- drop_iList m n
    return (v1, v2)
    

-----------------------------------------------------
len_iList :: IVar (IList a) -> Par Int
len_iList = go 0 where
    go n m = do
              list <- get m
              case list of
                Nil       -> return n
                Cons _ xs -> go (n+1) xs

-----------------------------------------------------
take_iList :: NFData a => IVar (IList a) -> Int -> Par (IVar (IList a))
take_iList m n = do
    v <- new
    fork (putList v m n)
    return v
    
  where
    putList v m 0 = put v Nil
    putList v m n = do
        list <- get m
        case list of
            Nil       -> put v Nil
            Cons x xs -> do
                          nv <- new
                          put v (Cons x nv)
                          putList nv xs (n-1)
-----------------------------------------------------
drop_iList :: NFData a => IVar (IList a) -> Int -> Par (IVar (IList a))
drop_iList m n = do
    v <- new
    fork (putList v m n)
    return v
    
  where
    putList v m 0 = do
        list <- get m
        case list of
            Nil       -> put v Nil
            Cons x xs -> do
                          nv <- new
                          put v (Cons x nv)
                          putList nv xs 0

    putList v m n = do
        list <- get m
        case list of
            Nil       -> put v Nil
            Cons x xs -> putList v xs (n-1) 
-----------------------------------------------------
-----------------------------------------------------







-----------------------------------------------------
-----------------------------------------------------
-- pipe1x :: [Int] -> Par Int
pipe1x param = toilist [1..10000]
  where
-----------------------------------------------------
    toilist m = do
        v <- new
        fork (putList v m)
        cks <- chunk_iList v param
        
        ab <- mapM (sfold 0) cks
        
        return (sum ab)

    putList v []      = put v Nil
    putList v (x:xs)  = do
                         nv <- new
                         put v (Cons x nv)
                         putList nv xs
    -----------------------------------------------------
    sfold acc m = acc `seq` do
        list <- get m
        case list of 
            Cons x xs -> sfold (junta acc (resolve x)) xs
            Nil -> return acc
-----------------------------------------------------
-----------------------------------------------------
-- $> runPar $ pipe1x [1..1000]













-----------------------------------------------------
-----------------------------------------------------
-- pipe1g :: [Ingeger] -> Par Int
pipe1g :: Int -> Par Int
pipe1g n = toilist $ chunkList n [1..10000]
  where
-----------------------------------------------------
    toilist [m] = return $ some resolve junta m
    toilist (m:ms) = do
        v <- new
        fork (putList v m)
        c <- sfold 0 v
        cs <- toilist ms
        return (junta c cs)

    putList v []      = put v Nil
    putList v (x:xs)  = do
                         nv <- new
                         put v (Cons x nv)
                         putList nv xs
    -----------------------------------------------------
    sfold acc m = acc `seq` do
        list <- get m
        case list of 
            Cons x xs -> sfold (junta acc (resolve x)) xs
            Nil -> return acc
    
    -----------------------------------------------------
    chunkList = go  where
        go _ [] = []
        go 1 xs = [xs]
        go n xs = f $ split (len xs `div` n) xs
            where 
                f (as, bs) = as : go (n-1) bs
                split = splitAt
                len = length

    -----------------------------------------------------
    some :: (a -> b) -> (b -> b -> b) -> [a] -> b
    some f g [x] = f x
    some f g (x:xs) = (g . f) x (some f g xs)
                
-----------------------------------------------------
-----------------------------------------------------
-- $> runPar $ pipe1g [1..100]










-----------------------------------------------------
-----------------------------------------------------
main :: IO ()
main = do
    putStrLn "...."
  -- teste0: isprime funcionando? ok
  -- print $ isprime 127
  
{-  teste1 pipe0
    time   35.469s  ( 20.162s elapsed)
    GC, MUT, residency, heap, arquivo eventlog... tudo muito alto
   
    print $ runPar (pipe0 [1..10000000])
-} 
  
{-  teste2 pipe0a  
    time   32.312s  ( 15.711s elapsed)
    ganho significativo de tempo e CG
   
    print $ runPar (pipe0a junta resolve [1..10000000])
-} 
  
  
{-  teste3 pipe1 (agora com iList)
    time   22.938s  ( 16.865s elapsed)
    MUT aumentou, GC, residency e slop diminuiram
    arquivo eventlog muito menor
    bangpattern sem mudança significativa
   
    print $ runPar (pipe1 [1..10000000])
-} 
  
  
{-  teste4 pipe2 (agora com um estágio de map)
    time   25.062s  ( 17.438s elapsed)
    tudo aumentou. até o arquivo
    falta do bang altera slop e GC siginificativamente
   
    print $ runPar (pipe2 [1..10000000])
-} 
  
{-  teste5 pipe3 (rate limited producer)
    time   18.625s  ( 15.190s elapsed)
    arquivo gigante, mas CG, Slop e residency reduzidos drasticamente
    bang claramente fazendo diferenca nesses itens.
    aumento de MUT
    n regula o CG, até um limite.
    
-} 

    print $ runPar (pipe3 200 [1..10000000])
  
  








-----------------------------------------------------
-- Bench --------------------------------------------
params :: [Int]
params = [func i | i <- [1..10] ]
func i = i


taskIO bla cmd = do 
           t0 <- getCurrentTime
           print bla
           print $ runPar $ cmd
           t1 <- getCurrentTime
           print (diffUTCTime t1 t0)
           putStrLn "---------------------"


-- utils --------------------------------------------
replicateM  :: Par (IVar Int) -> [Integer] -> Par [IVar Int]
replicateM mx = go where
    go [] = return []
    go (c:cs) = do 
                  x <- mx
                  xs <- go cs
                  return (x:xs)
