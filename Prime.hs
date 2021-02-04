module Prime where

import System.Process (system)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Control.DeepSeq (NFData, rnf)
import Control.Monad.Par
-- import EstudoPar

-----------------------------------------------------
-----------------------------------------------------
data IList a = Nil | Cons a (IVar (IList a)) | Forque (Par ()) (IList a)

instance NFData a => NFData (IList a) where
  rnf Nil = ()
  rnf (Cons a b) = rnf a `seq` rnf b
  rnf (Forque a b) = a `seq` rnf b
-----------------------------------------------------
-----------------------------------------------------


-----------------------------------------------------
-----------------------------------------------------
main = do
        system "clear"
        t0 <- getCurrentTime
        -- print $ runPar $ reduce4 [1..100000] -- ghci
        print $ runPar $ pipe7c [1..10000000] -- compile
        t1 <- getCurrentTime
        print (diffUTCTime t1 t0)
-----------------------------------------------------
-----------------------------------------------------






-----------------------------------------------------
-- Serial lazy isprime ------------------------------
isprime :: Integer -> Bool
isprime n | n < 2 = False
          | otherwise = null $
                filter (\p -> mod n p == 0) $
                    takeWhile (<= iSqr n) lazyPrimes

lazyPrimes :: [Integer]
lazyPrimes = 2: 3: filter isprime (interleave [5, 11..] [7, 13..])
-----------------------------------------------------
-----------------------------------------------------





-----------------------------------------------------
-- Serial prime range -------------------------------
primerange = go where
    go []     = 0
    go (x:xs) = (resolve x) + go xs
-----------------------------------------------------
reduce4 :: [Integer] -> Par Int
reduce4 m = go (particiona 16 m) where
    go [a] = return (primerange a)
    go xss = do
        let (as, bs) = divideBy2 xss
        v <- spawn (go bs)
        a <- go as
        b <- get v
        return (junta a b)
-----------------------------------------------------
-----------------------------------------------------





-----------------------------------------------------
-----------------------------------------------------
pipe5 :: [Integer] -> Par Int
pipe5 m = go (particiona 16 m) where
    go xss = do
        vars <- replicateM new xss
        mapM_ (\(v, a) -> fork $ put v (primerange a)) (zip vars xss)
        ys <- mapM get vars
        return (foldr junta 0 ys)
-----------------------------------------------------
replicateM :: Par (IVar Int) -> [[Integer]] -> Par [(IVar Int)]
replicateM mx = go where
    go [] = return []
    go (c:cs) = do
                 x  <- mx
                 xs <- go cs
                 return (x:xs)
-----------------------------------------------------
-----------------------------------------------------
        



-----------------------------------------------------
-----------------------------------------------------
pipe6a :: [Integer] -> Par Int
pipe6a m = sfold 0 =<< (toilist (particiona 16 m))
-----------------------------------------------------
  where
    toilist :: NFData a => [a] -> Par (IVar (IList a))
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
    sfold :: Int -> IVar (IList [Integer]) -> Par Int
    sfold acc m = acc `seq` do
        list <- get m
        case list of 
            Nil -> return acc
            Cons x xs -> sfold (junta acc (primerange x)) xs
-----------------------------------------------------
-----------------------------------------------------
        






-----------------------------------------------------
-----------------------------------------------------
pipe6b :: [Integer] -> Par Int
pipe6b m = sfold 0 =<< (toilist (particiona 16 m))
-----------------------------------------------------
  where
    toilist :: [[Integer]] -> Par (IVar (IList Int))
    toilist m = do
        v <- new
        fork $ loop m v
        return v

      where
        loop [] v     = put v Nil
        loop (x:xs) v = do
                         nv <- new
                         put v $ Cons (primerange x) nv
                         loop xs nv
    -----------------------------------------------------
    sfold :: Int -> IVar (IList Int) -> Par Int
    sfold acc m = acc `seq` do
        list <- get m
        case list of 
            Nil -> return acc
            Cons x xs -> sfold (junta acc x) xs
-----------------------------------------------------
-----------------------------------------------------
        





-----------------------------------------------------
-----------------------------------------------------
pipe7 :: [Integer] -> Par Int
pipe7 m = sfold 0 =<< (toilist (particiona 16 m))
-----------------------------------------------------
  where
    toilist :: [[Integer]] -> Par (IVar (IList Int))
    toilist m = do
        v <- new
        fork $ loop 0 m v
        return v

      where
        loop _ [] v     = put v Nil

        loop 0 (x:xs) v = do
                         nv <- new
                         put v $ Forque (loop 0 xs nv) (Cons (primerange x) nv)

        loop n (x:xs) v = do
                         nv <- new
                         put v $ Cons (primerange x) nv
                         loop (n-1) xs nv
    -----------------------------------------------------
    sfold :: Int -> IVar (IList Int) -> Par Int
    sfold acc m = acc `seq` do
        list <- get m
        case list of 
            Nil -> return acc
            Cons x xs -> sfold (junta acc x) xs
            Forque op (Cons x xs) -> fork op >> sfold (junta acc x) xs
            
-----------------------------------------------------
-----------------------------------------------------
        




-----------------------------------------------------
-----------------------------------------------------
pipe7b :: [Integer] -> Par Int
pipe7b m = sfold 0 =<< smap =<< (toilist (particiona 16 m))
-----------------------------------------------------
  where
    -- toilist :: [[Integer]] -> Par (IVar (IList Int))
    toilist m = do
        v <- new
        fork $ loop 200 m v
        return v

      where
        loop _ [] v     = put v Nil

        loop 0 (x:xs) v = do
                         nv <- new
                         put v $ Forque (loop 200 xs nv) (Cons x nv)

        loop n (x:xs) v = do
                         nv <- new
                         put v $ Cons x nv
                         loop (n-1) xs nv
    
    -----------------------------------------------------
    smap m = do
        v <- new
        fork $ loop m v
        return v
      
      where
        loop m v = do
            list <- get m
            
            case list of 
                Nil                   -> put v Nil
                Cons x xs             -> consFx v x xs 
                Forque op (Cons x xs) -> fork op >> consFx v x xs 
                
        consFx v x xs = do
                          nv <- new
                          put v $ Cons (primerange x) nv
                          loop xs nv

    -----------------------------------------------------
    -- sfold :: Int -> IVar (IList Int) -> Par Int
    sfold acc m = acc `seq` do
        list <- get m
        case list of 
            Nil -> return acc
            Cons x xs -> sfold (junta acc x) xs
            Forque op (Cons x xs) -> fork op >> sfold (junta acc x) xs
            
-----------------------------------------------------
-----------------------------------------------------
        




-----------------------------------------------------
-----------------------------------------------------
pipe7c :: [Integer] -> Par Int
pipe7c m = do
            let a = particiona 32 m
            
            (ilist12, ilist11) <- toilist a

            ilist11' <- smap ilist11
            
            n2 <- sfold2 0 ilist12  "sem smap"
            n1 <- sfold1 0 ilist11' "com smap"
            
            return (n1+n2)
-----------------------------------------------------
  where
    -- toilist :: [[Integer]] -> Par (IVar (IList Int), IVar (IList [Integer]))
    toilist m = go (divideBy2 m)
    go (m1, m2) = do
        v <- new
        u <- new
        fork $ loop m1 u 
        fork $ loop m2 v 
        return (u, v)

      where
        loop [] v     = do
                          put v Nil

        loop (x:xs) v = do
                         nv <- new
                         put v $ Cons x nv
                         loop xs nv
    
    -----------------------------------------------------
    smap :: IVar (IList [Integer]) -> Par (IVar (IList Int))
    smap m = do
        v <- new
        fork $ loop m v
        return v
      
      where
        loop m v = do
            list <- get m
            
            case list of 
                Nil                   -> put v Nil
                Cons x xs             -> consFx v x xs 
                Forque op (Cons x xs) -> fork op >> consFx v x xs 
                
        consFx v x xs = do
                          nv <- new
                          put v $ Cons (primerange x) nv
                          loop xs nv

    -----------------------------------------------------
    -- sfold :: Int -> IVar (IList Int) -> String -> Par Int
    sfold1 acc m s = acc `seq` do
        list <- get m
        case list of 
            Nil -> do
                    -- msg s
                    return acc
            Cons x xs -> sfold1 (junta acc x) xs s
    -----------------------------------------------------
    sfold2 :: Int -> IVar (IList [Integer]) -> String -> Par Int
    sfold2 acc m s = acc `seq` do
        list <- get m
        case list of 
            Nil -> do
                    -- msg s
                    return acc
            Cons x xs -> sfold2 (junta acc (primerange x)) xs s
            
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
type Junta a = a -> a -> a
type Resolve b a = b -> a

junta :: Junta Int
junta = (+)
-- junta a b = 2*a + b

resolve :: Resolve Integer Int
resolve p | isprime p = 1
          | otherwise = 0
-----------------------------------------------------
-----------------------------------------------------












-----------------------------------------------------------
-- Utils --------------------------------------------------
iSqr = floor . sqrt . fromIntegral

interleave (x:xs) ys = x:interleave ys xs

divideBy :: Int -> [a] -> ([a], [a])
divideBy n m = splitAt (length m `div` n)  m
divideBy2 m = divideBy 2 m

particiona :: Int -> [a] -> [[a]]
particiona n m = go m n where
    go _ 0 = []
    go m n = a : go b (n-1) where
        (a, b) = splitAt (length m `div` n) m
-----------------------------------------------------------
-----------------------------------------------------------