module Main where

import Sokoban
import Rep0

import System.Process (system)
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime (..))

import Control.Concurrent.Async
import Control.Exception (finally, bracket)


-----------------------------------------------------
-----------------------------------------------------
type Jobs     = [(Moves, State)]
type Box      = (Maybe State, Set State)
-- type Pacote = Set Jobs

-- solver :: UTCTime -> State -> Int -> IO (Maybe State)
solver t t0 st p2 dbug = do
    visit    <- newMyVar (eOrd st)
    bfs 1 visit [st] 
   
  where -- escopo solver
    bfs iter visit sts  = do

        -- debug
        debug dbug iter t0 jobs sts
        if iter > 150 then do return Nothing 
        else do 

            --  main loop
            if t == 1 then do
                r <- loopIO jobs
                case r of
                    (Just x, _)   -> return (Just x)
                    (Nothing, stSet') -> bfs iter' visit (tolist stSet')

            else do
                r <- loop2 jobs
                case r of
                    (Just x, _)   -> return (Just x)
                    (Nothing, stSet') -> bfs iter' visit (tolist stSet') 


      where  -- escopo bfs
        iter' = iter + 1
        jobs  = [(m, s) | s  <- sts, m  <- actions]

        
 
                                

        ----------------------------------------------------
        ----------------------------------------------------
        loopIO :: Jobs -> IO Box
        loopIO m = 
            driver (particiona p2 m) 

              where
                driver :: [Jobs] -> IO Box
                driver = go (setNew st) where
                    go stSet' []              = return (Nothing, stSet')
                    go stSet (xs:xss)         = do
                        r <- subloop xs
                        case r of                                                   
                            (Just cm, _) -> return r
                            (Nothing, stSet') -> 
                                go (merge stSet stSet') xss
                                                                     
                subloop :: Jobs -> IO Box
                subloop = go (setNew st) where
                    go stSet' []     = return (Nothing, stSet')
                    go stSet (x:xs) = do
                        case (funcSucess x) of
                            Goal cm     -> return (Just cm, Empty)
                            Invalid     -> go stSet xs
                            Factible cm -> do

                                look <- lookupMyVar visit (eOrd cm)
                                if look then 
                                    go stSet xs
                                else do

                                    putMyVar visit (eOrd cm)
                                    go (insert stSet cm) xs

                                
        ----------------------------------------------------
        ----------------------------------------------------
        loop2 :: Jobs -> IO Box
        loop2 m = 
            asyncLoop (particiona p2 m) 

              where
                asyncLoop :: [Jobs] -> IO Box
                asyncLoop = go []    where
                    go asyncs []               = waitLoop asyncs
                    go asyncs (xs:xss)         =  
                        withAsync (subloop xs) $ \a -> 
                            go (a:asyncs) xss 

                waitLoop :: [Async Box] -> IO Box
                waitLoop = go (setNew st) where
                    go stSet' []              = return (Nothing, stSet')
                    go stSet (xs:xss)         = do
                        r <- wait xs
                        case r of                                                   
                            (Just cm, _) -> return r
                            (Nothing, stSet') -> 
                                go (merge stSet stSet') xss        -- main loop list        
                                                                     
                subloop :: Jobs -> IO Box
                subloop = go (setNew st) where
                    go stSet' []     = return (Nothing, stSet')
                    go stSet (x:xs) = do
                        case (funcSucess x) of
                            Goal cm     -> return (Just cm, Empty)
                            Invalid     -> go stSet xs
                            Factible cm -> do

                                look <- lookupMyVar visit (eOrd cm)
                                if look then 
                                    go stSet xs
                                else do

                                    putMyVar visit (eOrd cm)
                                    go (insert stSet cm) xs

                                
        ----------------------------------------------------
                        

{----------------------------------------------------

                                look <- lookupMyVar visit (eOrd cm)
                                if look then 
                                    go stSet xs
                                    
                                else do
                                    putMyVar visit (eOrd cm)
                                    go (insert stSet cm) xs


                subloop :: Jobs -> IO Box
                subloop = go (setNew st) where
                    go stSet' []     = subsubloop stSet'
                    go stSet (x:xs) = do
                        case (funcSucess x) of
                            Goal cm     -> return (Just cm, Empty)
                            Invalid     -> go stSet xs
                            Factible cm -> go stSet xs
                
                
                subsubloop = go where
                    go Empty = return (Nothing, stSet')
                    go a = return (Nothing, stSet')





                                look <- lookupMyVar visit (eOrd cm)
                                if look then 
                                    go stSet xs
                                else do

                                    putMyVar visit (eOrd cm)
                                    go (insert stSet cm) xs
----------------------------------------------------}



actions :: [Moves]
actions = [toEnum 0 ..]
    
particiona :: Int -> [a] -> [[a]]
particiona n m = go m n where
    go _ 0 = []
    go m n = a : go b (n-1) where
        (a, b) = splitAt (length m `div` n) m

particiona2 n m = fromList Empty (particiona n m) 
    















-----------------------------------------------------
-----------------------------------------------------
debug dbug iter t0 sts sts' = do
    if not dbug then do
        return ()
    else do
        a <- getCurrentTime
        print (iter, diffUTCTime a t0, length sts, length sts')





-----------------------------------------------------
ret0f :: UTCTime -> IO (Maybe State)
ret0f t0 = do
    solver 1 t0 testSt 36 True

--------------------------
main :: IO ()
main = do
        system "clear"
        t0 <- getCurrentTime
        r <- ret0f t0
        print r
        t1 <- getCurrentTime
        print (diffUTCTime t1 t0)


-----------------------------------------------------

taskIO t p2 = do
    t0 <- getCurrentTime
    solver t t0 testSt p2 False
    t1 <- getCurrentTime
    print (diffUTCTime t1 t0)

-----------------------------------------------------







{----------------------------------------------------
    Arvores podem ser usadas para representar conjuntos em uma classe.
    E essa classe pode ser usada para representar 
    mapas ou hash tables, entre outras estruturas de dados.
    
    ...enumerando ou aplicando a fun??o de interesse
    ...bfs usando fila tem complexidade O(n?)  o ideal seria O(n)
    ...? possivel compor uma fila com tempo constante
----------------------------------------------------}

bfnJG t = t' where
    go (ks, Empty)        = (ks, Empty)
    go (k:ks0, Bin x a b) = (k+1 : ks2, Bin (x, k) a' b')
        where
            (ks1, a') = go (ks0, a)
            (ks2, b') = go (ks1, b)

    (ks, t')              = go (1:ks, t)


teste3 = bfnJG (Bin 'a' (Bin 'a' Empty Empty) (Bin 'a' Empty Empty))




-- 11.7 - Programação Funcional em Haskell: Monad State
-- https://youtu.be/_yKJ2ft9Lg4?list=PLYItvall0TqJ25sVTLcMhxsE0Hci58mpQ






newSemaf2 :: e -> ST e ()
newSemaf2 n = ST (\m -> ((), n))


obtemTicket2 :: ST Int Bool
obtemTicket2 = ST cont where
    cont m | m == 0    = (False, 0)
           | otherwise = (True, m - 1)
           
liberaSemaf2 :: ST Int ()
liberaSemaf2 = ST (\m -> ((), m + 1))



--------------------------------------------
--------------------------------------------
newtype ST e a = ST {runstate :: (e -> (a, e))}
--------------------------------------------
instance Monad (ST e) where
   ST g >>= h = ST (\e ->
                    let (x, e') = g e
                        ST f = h x
                        
                    in  f e')
--------------------------------------------
instance Applicative (ST e)  where
    pure a = ST (\e -> (a, e))
    stf <*> stx = do
                    f <- stf
                    fmap f stx
--------------------------------------------
instance Functor (ST s) where
    fmap f st = do
                  x <- st
                  return (f x)
--------------------------------------------
--------------------------------------------


