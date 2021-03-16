module Pipe where
import System.Process (system)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.DeepSeq (NFData, rnf)

import Sokoban
import Rep0
import Control.Monad.Par


-----------------------------------------------------
-----------------------------------------------------
main :: IO ()
main = do
        system "clear"
        t0 <- getCurrentTime

        print $ runPar $ pipe8 36 [1..10000000]
        
        -- print testSt
        
        t1 <- getCurrentTime
        print (diffUTCTime t1 t0)
-----------------------------------------------------
-----------------------------------------------------



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
-- pipe de stream básico, sem partição nem map 
-----------------------------------------------------
pipe6 :: Int -> [Integer] -> Par Int
pipe6 r m = sfold 0 =<< toilist m
-----------------------------------------------------
  where
    toilist m = do
        v <- new
        fork (loop m v)
        return v

    loop m v = do
        case m of
            
            [] -> put v Nil 
            
            (x:xs) -> do 
                nv <- new
                put v (Cons x nv)
                loop xs nv

    sfold acc m =
        case acc of
            _ -> do
                list <- get m
                case list of 
                    Nil -> return acc
                    Cons x xs -> sfold (bfs acc (solv x)) xs
-----------------------------------------------------
-----------------------------------------------------



solv = resolve
bfs  = junta
{-
------------------------------
solv x = x

bfs acc sts = 
    let actions = [toEnum 0 ..]
        jobs    = [(m, s) | s <- sts, m <- actions]

-}        
        
        











































-----------------------------------------------------
-- pipe primitivo
-----------------------------------------------------
pipe5a :: Int -> [Integer] -> Par Int
pipe5a r m = go (particiona r m) where
    go xss = do
        ivars <- mapM (spawnP . resolveJunta) xss
        ys    <- mapM get ivars
        return (foldr junta 0 ys)
-----------------------------------------------------
-----------------------------------------------------







-----------------------------------------------------
-- com partição
-----------------------------------------------------
pipe6a :: Int -> [Integer] -> Par Int
pipe6a r m = sfold 0 =<< (toilist (particiona r m))
-----------------------------------------------------
  where
    toilist :: NFData a => [a] -> Par (IVar (IList a))
    toilist m = do
        v <- new
        fork (loop m v)
        return v

    loop [] v     = put v Nil
    loop (x:xs) v = do
        nv <- new
        put v (Cons x nv)
        loop xs nv

    sfold :: Int -> IVar (IList [Integer]) -> Par Int
    sfold acc m = acc `seq` do
        list <- get m
        case list of 
            Nil -> return acc
            Cons x xs -> sfold (junta acc (resolveJunta x)) xs
-----------------------------------------------------
-----------------------------------------------------
        





-----------------------------------------------------
-- com partição e rate limit
-----------------------------------------------------
pipe6cb r m = sfold 0 =<< toilist (particiona (16*r) m)
  where
    --------------------------------------------
    toilist m = do
        v <- new
        fork (loop r m v)
        return v

    loop _ [] v = put v Nil

    loop 0 (x:xs) v = do
        nv <- new
        put v (Forque (loop r xs nv) (Cons x nv)) 
        
    loop n (x:xs) v = do
        nv <- new
        put v (Cons x nv)
        loop (n-1) xs nv
    
    ----------------------------------------------
    sfold acc m = acc `seq` do
        list <- get m
        case list of 
            Nil                   -> return acc
            Cons x xs             -> rec acc x xs
            Forque op (Cons x xs) -> fork op >> rec acc x xs
    
    rec acc x xs = sfold (junta acc (resolveJunta x)) xs
    ----------------------------------------------
            
-----------------------------------------------------
        









-----------------------------------------------------
-- versão especial, com rate limit e map, sem partição
-----------------------------------------------------
pipe7b0 r m = sfold 0 =<< smap =<< toilist m
-----------------------------------------------------
  where
    toilist m = do
        v <- new
        fork (loop r m v)
        return v

      where
        loop _ [] v     = put v Nil

        loop 0 (x:xs) v = do
            nv <- new
            put v (Forque (loop r xs nv) (Cons x nv))

        loop n (x:xs) v = do
            nv <- new
            put v (Cons x nv)
            loop (n-1) xs nv
    
    -----------------------------------------------------
    smap m = do
        v <- new
        fork (loop m v)
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
-- rate limit, map e partição
-----------------------------------------------------
pipe7a r m = sfold 0 =<< smap =<< toilist (particiona (4*r) m)
  where
    --------------------------------------------
    toilist m = do
        v <- new
        fork (loop r m v)
        return v

    loop _ [] v = put v Nil

    loop 0 (x:xs) v = do
        nv <- new
        put v (Forque (loop r xs nv) (Cons x nv))   

    loop n (x:xs) v = do
        nv <- new
        put v (Cons x nv)
        loop (n-1) xs nv
    
    ----------------------------------------------
    smap m = do
        v <- new
        fork (loop m v)
        return v
      
      where
        loop m v = do
            list <- get m
            case list of 
                Nil                   -> put v Nil 
                Cons x xs             -> consSmap v x xs
                Forque op (Cons x xs) -> fork op >> consSmap v x xs
                
        consSmap v x xs = do
            nv <- new
            put v (Cons (resolveJunta x) nv)
            loop xs nv

    ---------------------------------------------- -- pipe 7
    sfold acc m = acc `seq` do
        list <- get m
        case list of 
            Nil                   -> return acc
            Cons x xs             -> rec acc x xs
            Forque op (Cons x xs) -> fork op >> rec acc x xs
    
    rec acc x xs = sfold (junta acc x) xs            
    

-----------------------------------------------------
-----------------------------------------------------
        
















-----------------------------------------------------
-----------------------------------------------------
pipe8 r m = do
    list_of_ilists  <- mapM (toilist . (: [])) (particiona r m)
    list_of_ilists' <- mapM smap list_of_ilists
    list_of_results <- mapM (sfold 0) list_of_ilists'
    return (foldr junta 0 list_of_results)
    
  where
    --------------------------------------------
    toilist m = do
        v <- new
        fork (loop r m v)
        return v

    loop _ [] v = put v Nil

    loop 0 (x:xs) v = do
        nv <- new
        put v (Forque (loop r xs nv) (Cons x nv))

    loop n (x:xs) v = do
        nv <- new
        put v (Cons x nv)
        loop (n-1) xs nv
    
    ----------------------------------------------
    smap m = do
        v <- new
        fork (loop m v)
        return v
      
      where
        loop m v = do
            list <- get m
            case list of 
                Nil                   -> put v Nil
                Cons x xs             -> consSmap v x xs
                Forque op (Cons x xs) -> fork op >> consSmap v x xs
                
        consSmap v x xs = do
            nv <- new
            put v (Cons (resolveJunta x) nv)
            loop xs nv

    ----------------------------------------------
    sfold acc m = acc `seq` do
        list <- get m
        case list of 
            Nil                   -> return acc
            Cons x xs             -> rec acc x xs
            Forque op (Cons x xs) -> fork op >> rec acc x xs
    
    rec acc x xs = sfold (junta acc x) xs            
    

-----------------------------------------------------
-----------------------------------------------------
        









-----------------------------------------------------
-- Utils --------------------------------------------
isprime :: Integer -> Bool
isprime n | n < 2 = False
          | otherwise = null $
                filter (\p -> mod n p == 0) $
                    takeWhile (<= iSqr n) lazyPrimes

lazyPrimes :: [Integer]
lazyPrimes = 2: 3: filter isprime (interleave [5, 11..] [7, 13..])

-----------------------------------------------------
type Junta b = b -> b -> b
type Resolve a b =  a -> b

junta :: Junta Int
junta = (+)

resolve :: Resolve Integer Int
resolve p | isprime p = 1
          | otherwise = 0

-----------------------------------------------------
resolveJunta :: [Integer] -> Int
resolveJunta = some resolve junta

some :: (a -> b) -> (b -> b -> b) -> [a] -> b
some f g [x] = f x
some f g (x:xs) = (g . f) x (some f g xs)

-- resolveJunta = go where
--     go []     = 0
--     go (x:xs) = junta (resolve x) (go xs)

particiona :: Int -> [a] -> [[a]]
particiona n m = go m n where
    go _ 0 = []
    go m n = a : go b (n-1) where
        (a, b) = splitAt (length m `div` n) m

iSqr :: Integer -> Integer
iSqr = floor . sqrt . fromIntegral

interleave :: [a] -> [a] -> [a]
interleave (x:xs) ys = x:interleave ys xs








-----------------------------------------------------
-----------------------------------------------------
taskIO0 r = do
        t0 <- getCurrentTime
        print $ runPar $ reduce2 r [1..100000]
        t1 <- getCurrentTime
        print (diffUTCTime t1 t0)

taskIO1 r = do
        t0 <- getCurrentTime
        print $ runPar $ pipe8 r [1..100000]
        t1 <- getCurrentTime
        print (diffUTCTime t1 t0)

taskIO2 r = do
        t0 <- getCurrentTime
        print $ runPar $ pipe8 r [1..100000]
        t1 <- getCurrentTime
        print (diffUTCTime t1 t0)
-----------------------------------------------------
reduce2 :: Int -> [Integer] -> Par Int
reduce2 r m = go (particiona r m) where
    go [a] = return (resolveJunta a)
    go xs = do
            v <- spawn (go bs)
            a <- go as
            b <- get v
            return (junta a b)
      where
        [as, bs] = particiona 2 xs
-----------------------------------------------------






-- stack exec pipe --RTS -- +RTS -N -ls -s
