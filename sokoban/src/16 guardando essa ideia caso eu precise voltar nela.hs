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
solver t t0 st p2 = do
    visit    <- newMyVar (eOrd st)
    bfs 1 visit [st] 
   
  where -- escopo solver
    bfs iter visit sts  = do

        -- debug
        -- debug iter t0 jobs sts
        if iter > 150 then do return Nothing 
        else do 

            --  main loop
            if t == 1 then do
                r <- loop1 jobs
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
        -- loop baseado em arvore binária 
        ----------------------------------------------------
        loop1 :: Jobs -> IO Box
        loop1 m = do 
            (maybeRes, stSet) <- go (setNew (True, st)) (particiona2 p2 m) 
            return (maybeRes, map1 snd stSet)          
          where 
            go stSet = asyncLoop 
              where
                -- asyncLoop :: Set Jobs -> IO Box 
                asyncLoop :: Set Jobs -> IO (Maybe State, Set (Bool, State))
                asyncLoop = go Empty where
                    go asyncs Empty            = waitLoop stSet asyncs
                    go asyncs (Bin xs esq dir) =  
                        withAsync (subloop stSet xs) $ \a -> 
                            go (insert asyncs a) (merge esq dir)

                -- waitLoop :: Set (Bool, State) -> Set (Async Box) -> IO Box
                waitLoop :: Set a -> Set (Async (Maybe b, Set a)) -> IO (Maybe b, Set a)
                waitLoop stSet' Empty           = return (Nothing, stSet')
                waitLoop stSet (Bin xs esq dir) = do
                    r <- wait xs
                    case r of                                                   
                        (Just cm, _) -> return r                     
                        (Nothing, stSet') -> do                       
                            let stSet'' = merge stSet stSet'         -- main loop arv
                            waitLoop stSet'' esq                      
                            waitLoop stSet'' dir                              

                ------------------------------------------------
                -- subloop :: Set State -> Jobs -> IO Box
                subloop :: Set (Bool, State) -> Jobs -> IO (Maybe State, Set (Bool, State))
                subloop stSet' []     = return (Nothing, stSet')
                subloop stSet (x:xs) = do
                    case (funcSucess x) of
                        Goal cm     -> return (Just cm, Empty)
                        Invalid     -> subloop stSet xs
                        Factible cm -> do

                            look <- lookupMyVar visit (eOrd cm)
                            if look then 
                                subloop stSet xs
                            else do

                                putMyVar visit (eOrd cm)
                                subloop (insert stSet (True, cm)) xs
          ----------------------------------------------------
        -- loop baseado em arvore binária 
        ----------------------------------------------------
        loop2 :: Jobs -> IO Box
        loop2 m = do 
            (maybeRes, stSet) <- go (setNew (True, st)) (particiona2 p2 m) 
            return (maybeRes, map1 snd stSet)          
          where 
            go stSet = asyncLoop 
              where
                -- asyncLoop :: Set Jobs -> IO Box 
                asyncLoop :: Set Jobs -> IO (Maybe State, Set (Bool, State))
                asyncLoop = go Empty where
                    go asyncs Empty            = waitLoop stSet asyncs
                    go asyncs (Bin xs esq dir) =  
                        withAsync (subloop stSet xs) $ \a -> 
                            go (insert asyncs a) (merge esq dir)

                -- waitLoop :: Set (Bool, State) -> Set (Async Box) -> IO Box
                waitLoop :: Set a -> Set (Async (Maybe b, Set a)) -> IO (Maybe b, Set a)
                waitLoop stSet' Empty           = return (Nothing, stSet')
                waitLoop stSet (Bin xs esq dir) = do
                    r <- wait xs
                    case r of                                                   
                        (Just cm, _) -> return r                     
                        (Nothing, stSet') -> do                       
                            let stSet'' = merge stSet stSet'         -- main loop arv
                            waitLoop stSet'' esq                      
                            waitLoop stSet'' dir                              

                ------------------------------------------------
                -- subloop :: Set State -> Jobs -> IO Box
                subloop :: Set (Bool, State) -> Jobs -> IO (Maybe State, Set (Bool, State))
                subloop = go where
                
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
                                    go (insert stSet (True, cm)) xs
                                
                                
        ----------------------------------------------------
        -- loop baseado em lista
        ----------------------------------------------------
        loop1a :: Jobs -> IO Box
        loop1a m = 
        
            go (setNew st) (particiona p2 m) 
          where
            go stSet = asyncLoop
              where
              
                asyncLoop :: [Jobs] -> IO Box
                asyncLoop = go []    where
                    go asyncs []               = waitLoop stSet asyncs
                    go asyncs (xs:xss)         =  
                        withAsync (subloop stSet xs) $ \a -> 
                            go (a:asyncs) xss 
                            

                waitLoop :: Set State -> [Async Box] -> IO Box
                waitLoop stSet' []              = return (Nothing, stSet')
                waitLoop stSet (xs:xss)         = do
                    r <- wait xs
                    case r of                                                   
                        (Just cm, _) -> return r
                        (Nothing, stSet') -> 
                            waitLoop (merge stSet stSet') xss        -- main loop list        
                                                                     
                            

                ------------------------------------------------
                subloop :: Set State -> Jobs -> IO Box
                subloop stSet' []     = return (Nothing, stSet')
                subloop stSet (x:xs) = do
                    case (funcSucess x) of
                        Goal cm     -> return (Just cm, Empty)
                        Invalid     -> subloop stSet xs
                        Factible cm -> do

                            look <- lookupMyVar visit (eOrd cm)
                            if look then 
                                subloop stSet xs
                            else do

                                putMyVar visit (eOrd cm)
                                subloop (insert stSet cm) xs

        
        
        
        ----------------------------------------------------









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
debug iter t0 sts sts' = do
    a <- getCurrentTime
    print (iter, diffUTCTime a t0, length sts, length sts')






ret0f :: UTCTime -> IO (Maybe State)
ret0f t0 = do
    solver 1 t0 testSt 24

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

taskIO1 t p2 = do
    t0 <- getCurrentTime
    solver 1 t0 testSt p2
    t1 <- getCurrentTime
    print (diffUTCTime t1 t0)

taskIO2 t p2 = do
    t0 <- getCurrentTime
    solver 2 t0 testSt p2
    t1 <- getCurrentTime
    print (diffUTCTime t1 t0)

taskIO t p2 = do
    t0 <- getCurrentTime
    solver t t0 testSt p2
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


