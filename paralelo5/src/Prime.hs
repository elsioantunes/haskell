module Prime where

import System.Process (system)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Control.DeepSeq (NFData, rnf)
-- import Control.Monad.Par
import EstudoPar

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
        -- r <- serial resolve junta [2.. 10000000]
        -- print r

        t0 <- getCurrentTime
        -- print $ runPar $ reduce4 [1..100000] -- ghci
        -- print $ runPar $ reduce2 16 [1..100000] -- ghci
        print $ runPar $ pipe7b0 289 [1..10000000] -- compile
        -- print $ last $ take 664579 lazyPrimes -- 9.7
        -- print $ length $ takeWhile (<10000000) lazyPrimes
        -- print $ serial resolve junta [2.. 10000000]
        -- print $ runPar $ reduce2 49 [1..10000] -- ghci
        
        t1 <- getCurrentTime
        print (diffUTCTime t1 t0)


taskIO = do
        r <- serial resolve junta [2.. 100000]
        print r
        

{----------------------------------------------------
taskIO0 r = do
        t0 <- getCurrentTime
        print $ runPar $ reduce0 [1..(100000+r)]
        t1 <- getCurrentTime
        print (diffUTCTime t1 t0)
        print r
----------------------------------------------------}

taskIO0 r = do
        t0 <- getCurrentTime
        print $ runPar $ reduce2 r [1..10000000]
        t1 <- getCurrentTime
        print (diffUTCTime t1 t0)

taskIO1 r = do
        t0 <- getCurrentTime
        print $ runPar $ pipe7b0 r [1..10000000]
        t1 <- getCurrentTime
        print (diffUTCTime t1 t0)

taskIO2 r = do
        t0 <- getCurrentTime
        print $ runPar $ pipe7a2 r [1..10000000]
        t1 <- getCurrentTime
        print (diffUTCTime t1 t0)

-----------------------------------------------------
-----------------------------------------------------




serial :: Show b => (a -> b) -> (b -> b -> b) -> [a] -> IO b
serial f g m = do
    t0 <- getCurrentTime
    let r = go m
    print r
    t1 <- getCurrentTime
    print (diffUTCTime t1 t0)
    return r  
  where
    go [x] = f x
    go (x:xs) = (g . f) x (go xs)




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
reduce1 :: Int -> [Integer] -> Par Int
reduce1 r = go  where -- range based
    go [a] = return (resolve a)
    
    go xs | length xs < r = do
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
-- nota para o uso de some e particiona
-----------------------------------------------------
reduce2 :: Int -> [Integer] -> Par Int
reduce2 r m = go (particiona r m) where -- partition
    go [a] = return (some resolve junta a)
    go xs = do
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
-- reduceG0 :: Int -> [Integer] -> Par Int
reduceG0 r m = go (particiona r m) where -- partition
    go [a] = return (some id junta a)
    go xs = do
            v <- spawn (go bs)
            a <- go as
            b <- get v
            msg $ show a ++ " + " ++ show b ++ " = " ++ show (a+b)
            return (junta a b)
      where
        (as, bs) = divideBy2 xs

-----------------------------------------------------
-----------------------------------------------------




spawn_ :: Par a -> Par (IVar a)
spawn_ p = do
  r <- new
  fork (p >>= put_ r)
  return r
        
-----------------------------------------------------
-----------------------------------------------------
-- reduce2Graph :: Int -> [Integer] -> Par Int
reduce2Graph r m = go 0 (particiona r m) where -- partition
    go gr [a] = do
               let ret = some resolve junta a
               msg $ show gr ++ " [color=silver; label="++ show ret ++"]"
               return (gr, ret)
    
    go gr xs = do
            v <- spawn_ (go (gr+2) bs)
            
            (gAs, a) <- go (gr+3) as
            (gBs, b) <- get v
            
            let sm = junta a b
                gra = (gAs*10)+(gBs*2)
            
            
            msg $ show gra ++ " [color=blue; label="++ show sm ++"]"
            msg $ show gAs ++ " -> " ++ show gra
            msg $ show gBs ++ " -> " ++ show gra
            
            return (gra, sm)
            
            
      where
        (as, bs) = divideBy2 xs
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

-- Serial prime range
primerange = go where
    go []     = 0
    go (x:xs) = junta (resolve x) (go xs)


some :: (a -> b) -> (b -> b -> b) -> [a] -> b
some f g [x] = f x
some f g (x:xs) = (g . f) x (some f g xs)        
-----------------------------------------------------
-----------------------------------------------------





-----------------------------------------------------
-- pipe5a fez esta versão parecer o monstro de Frankenstein 
-----------------------------------------------------
pipe5 :: Int -> [Integer] -> Par Int
pipe5 r m = go (particiona r m) where
    go xss = do
        ivars <- replicateM new xss
        mapM_ (\(v, a) -> fork $ put v (some resolve junta a)) (zip ivars xss)
        ys <- mapM get ivars
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
-- totalmente equivalente a pipe5 (?) sim! (comprovado pelo bench)
-----------------------------------------------------
pipe5a :: Int -> [Integer] -> Par Int
pipe5a r m = go (particiona r m) where
    go xss = do
        ivars <- mapM (spawnP . some resolve junta) xss
        ys    <- mapM get ivars
        return (foldr junta 0 ys)
-----------------------------------------------------
-----------------------------------------------------









-----------------------------------------------------
-----------------------------------------------------
-- pipe6c :: Int -> [Integer] -> Par Int
-- pipe6c :: Int -> [a] -> Par b
pipe6c r m = sfold 0 =<< toilist m
-----------------------------------------------------
  where
    -- toilist :: NFData a => [a] -> Par (IVar (IList a))
    toilist m = do
        v <- new
        fork (loop m v)
        return v

      where
        loop [] v     = put v Nil
        loop (x:xs) v = do
            nv <- new
            put v (Cons x nv)
            loop xs nv
    -----------------------------------------------------
    -- sfold :: Int -> IVar (IList [Integer]) -> Par Int
    sfold acc m = acc `seq` do
        list <- get m
        case list of 
            Nil -> return acc
            Cons x xs -> sfold (junta acc (resolve x)) xs
-----------------------------------------------------
-----------------------------------------------------
        



-----------------------------------------------------
-----------------------------------------------------

pipe6cs r m = sfold 0 =<< toilist m
-----------------------------------------------------
  where
    toilist m = do
        v <- new
        fork $ loop r m v
        return v

      where
        loop _ [] v     = put v Nil

        loop 0 (x:xs) v = do
                         nv <- new
                         put v $ Forque (loop 0 xs nv) (Cons (resolve x) nv)

        loop n (x:xs) v = do
                         nv <- new
                         put v $ Cons (resolve x) nv
                         loop (n-1) xs nv
    -----------------------------------------------------
    sfold acc m = acc `seq` do
        list <- get m
        case list of 
            Nil -> return acc
            Cons x xs -> sfold (junta acc x) xs
            Forque op (Cons x xs) -> fork op >> sfold (junta acc x) xs
            
-----------------------------------------------------
-----------------------------------------------------
        









-----------------------------------------------------
-- melhor que 6b
-----------------------------------------------------
pipe6a :: Int -> [Integer] -> Par Int
pipe6a r m = sfold 0 =<< (toilist (particiona r m))
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
                         put v (Cons x nv)
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
-- o 6a é melhor
-----------------------------------------------------
pipe6b :: Int -> [Integer] -> Par Int
pipe6b r m = sfold 0 =<< (toilist (particiona r m))
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
        









{-----------------------------------------------------
-----------------------------------------------------
pipe7a1 :: Int -> [Integer] -> Par Int
pipe7a1 r m = sfold 0 =<< (toilist (particiona (r*4) m))
-----------------------------------------------------
  where
    toilist :: [[Integer]] -> Par (IVar (IList Int))
    toilist m = do
        v <- new
        fork $ loop r m v
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
-----------------------------------------------------}
        
        
        


-----------------------------------------------------
-----------------------------------------------------
pipe7a2 ::  Int -> [Integer] -> Par Int
pipe7a2 r m = sfold 0 =<< (toilist (particiona (r*4) m))
-----------------------------------------------------
  where
    toilist :: [[Integer]] -> Par (IVar (IList [Integer]))
    toilist m = do
        v <- new
        fork $ loop r m v
        return v

      where
        loop _ [] v     = put v Nil

        loop 0 (x:xs) v = do
                         nv <- new
                         put v $ Forque (loop 0 xs nv) (Cons x nv)

        loop n (x:xs) v = do
                         nv <- new
                         put v $ Cons x nv
                         loop (n-1) xs nv
    -----------------------------------------------------
    sfold :: Int -> IVar (IList [Integer]) -> Par Int
    sfold acc m = acc `seq` do
        list <- get m
        case list of 
            Nil -> return acc
            Cons x xs -> sfold (junta acc (primerange x)) xs
            Forque op (Cons x xs) -> fork op >> sfold (junta acc (primerange x)) xs
            
-----------------------------------------------------
-----------------------------------------------------
        




-----------------------------------------------------
-----------------------------------------------------
pipe7b0 r m = sfold 0 =<< smap =<< toilist m
-----------------------------------------------------
  where
    toilist m = do
        v <- new
        fork $ loop r m v
        return v

      where
        loop _ [] v     = put v Nil

        loop 0 (x:xs) v = do
                         nv <- new
                         put v $ Forque (loop r xs nv) (Cons x nv)

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
                          put v $ Cons (resolve x) nv
                          loop xs nv

    -----------------------------------------------------
    sfold acc m = acc `seq` do
        list <- get m
        case list of 
            Nil -> return acc
            Cons x xs -> sfold (junta acc x) xs
            Forque op (Cons x xs) -> fork op >> sfold (junta acc x) xs
            
-----------------------------------------------------
-----------------------------------------------------
        





-----------------------------------------------------
-- equiparado a 6a em tempo
-----------------------------------------------------
pipe7b :: Int -> [Integer] -> Par Int
pipe7b r m = sfold 0 =<< smap =<< (toilist (particiona r m))
-----------------------------------------------------
  where
    -- toilist :: [[Integer]] -> Par (IVar (IList Int))
    toilist m = do
        v <- new
        fork $ loop r m v
        return v

      where
        loop _ [] v     = put v Nil

        loop 0 (x:xs) v = do
                         nv <- new
                         put v $ Forque (loop r xs nv) (Cons x nv)

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
pipe7c1 :: Int -> [Integer] -> Par Int
pipe7c1 r m = do
            let a = particiona r m
            
            (ilist12, ilist11) <- toilist a

            ilist11' <- smap ilist11
            ilist12' <- smap ilist12

            -- n2 <- sfold2 0 ilist12  "sem smap"
            n1 <- sfold1 0 ilist11'  "com smap"
            n2 <- sfold1 0 ilist12'  "com smap"
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
                    --msg s
                    return acc
            Cons x xs -> sfold1 (junta acc x) xs s
    -----------------------------------------------------
    sfold2 :: Int -> IVar (IList [Integer]) -> String -> Par Int
    sfold2 acc m s = acc `seq` do
        list <- get m
        case list of 
            Nil -> do
                    --msg s
                    return acc
            Cons x xs -> sfold2 (junta acc (primerange x)) xs s
            
-----------------------------------------------------
-----------------------------------------------------
        


-----------------------------------------------------
-----------------------------------------------------
pipe7c2 :: Int -> [Integer] -> Par Int
pipe7c2 r m = do
            let a = particiona r m
            
            (ilist12, ilist11) <- toilist a

            -- ilist11' <- smap ilist11
            
            n2 <- sfold2 0 ilist12  "sem smap"
            n1 <- sfold2 0 ilist11  "sem smap"
            
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
                    --msg s
                    return acc
            Cons x xs -> sfold1 (junta acc x) xs s
    -----------------------------------------------------
    sfold2 :: Int -> IVar (IList [Integer]) -> String -> Par Int
    sfold2 acc m s = acc `seq` do
        list <- get m
        case list of 
            Nil -> do
                    --msg s
                    return acc
            Cons x xs -> sfold2 (junta acc (primerange x)) xs s
            
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