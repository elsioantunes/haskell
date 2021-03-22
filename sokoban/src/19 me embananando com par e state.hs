{-# LANGUAGE BangPatterns #-}
module Main where

import Sokoban
import Rep0

import System.Process (system)
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime (..))

import Control.Concurrent.Async
import Control.Exception (finally, bracket)
import Control.Monad.Par
import Control.DeepSeq (NFData, rnf)



-----------------------------------------------------
-----------------------------------------------------
type Jobs     = [(Moves, State)]
type Box      = (Maybe State, Set State)

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
        ----------------------------------------------------
        loop1 :: Jobs -> IO Box
        loop1 m = 
            asyncLoop (particionaThresh p2 m) 

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




instance NFData a => NFData (Set a) where
    rnf Empty = ()
    rnf (Bin x a b) = rnf x `seq` rnf a `seq` rnf b

instance NFData (State) where  
    rnf a = rnf a -- não sei pq isso funciona. mas funciona




solver2 = undefined
-- solver2 :: State -> IO (Maybe State)


-- solver3 :: State -> Par a -> a
solver3 st = runPar $ do
    let ret = runstate (bfs [st]) (setNew $ eOrd st)
    v <- new 
    fork (put v ret)
    
    return v 
   
  where
    bfs :: [State] -> ST (Set ((Int, Int), (Int, Int))) (Maybe State)
    bfs sts = do
        r <- loop jobs
        
        case r of
            ( Just x,      _) -> return (Just x)
            (Nothing, stSet') -> bfs (tolist stSet')

      where
        jobs = [(m, s) | s  <- sts, m  <- actions]
        ----------------------------------------------------
        
        loop :: [(Moves, State)] -> ST (Set ((Int, Int), (Int, Int))) (Maybe State, Set State)
        loop m = driver (particionaThresh 1334 m) where
            driver = go (setNew st) where
                go stSet' []      = return (Nothing, stSet')
                go stSet  (xs:xss) = do
                    r <- subloop xs
                    case r of                                                   
                        (Just cm,      _) -> return (Just cm, Empty)
                        (Nothing, stSet') -> go (merge stSet stSet') xss

            subloop xs = go (setNew st) xs where
                go stSet' []    = return (Nothing, stSet')
                go stSet (x:xs) = do
                    case (funcSucess x) of
                        Goal cm     -> return (Just cm, Empty)
                        Invalid     -> go stSet xs
                        Factible cm -> do

                            look <- lookupVisit (eOrd cm)

                            if look then 
                                go stSet xs
                                
                            else do
                                updateVisit (eOrd cm)
                                go (insert stSet cm)  xs



updateVisit :: Ord a => a -> ST (Set a) ()
updateVisit x = ST (\visit -> ((), insert visit x))


lookupVisit :: Ord t => t -> ST (Set t) Bool
lookupVisit x = ST (\visit -> (member visit x, visit))










------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-- solver2 = 




------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
data Arv s = No s [Arv s] deriving Show

arvore :: ([Moves] -> State) -> Arv State
arvore f = No (f []) ramos where
    ramos = map ramifica [toEnum 0 ..]
    ramifica m = arvore (f' m) 
    f' m ms = f (m:ms)

indexa :: Arv State -> ([Moves] -> State)
indexa (No x ts) []     = x
indexa (No x ts) (m:ms) = indexa (ts !! fromEnum m) ms

arv = arvore (indexa (No testSt []))
testeIdx = print $ indexa arv [Cima]


------------------------------------------------------------------------------------






actions :: [Moves]
actions = [toEnum 0 ..]
    
particiona :: Int -> [a] -> [[a]]
particiona n m = go m n where
    go _ 0 = []
    go m n = a : go b (n-1) where
        (a, b) = splitAt (length m `div` n) m
 
 
particionaThresh :: Int -> [a] ->  [[a]]
particionaThresh n = go where
    go [] = []
    go xs = f $ splitAt n xs
        where 
            f (as, bs) = (as : go bs)





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
    -- solver 2 t0 testSt 1334 True
    solver2 testSt 

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










mklist []     = []
mklist ((a,b,c,d):xs) = c : mklist xs

tp = [
    (1,0,4,1),
    (2,0.0010022,152,38),
    (3,0.0020056,156,39),
    (4,0.0020056,156,39),
    (5,0.0020056,160,40),
    (6,0.0030094,172,43),
    (7,0.0030094,172,43),
    (8,0.0040125,172,43),
    (9,0.0040125,180,45),
    (10,0.0040125,176,44),
    (11,0.0040125,192,48),
    (12,0.0050153,212,53),
    (13,0.0050153,240,60),
    (14,0.0060194,280,70),
    (15,0.0060194,344,86),
    (16,0.0070223,444,111),
    (17,0.0080254,556,139),
    (18,0.009029,696,174),
    (19,0.0100327,808,202),
    (20,0.0110354,1044,261),
    (21,0.0130422,1200,300),
    (22,0.0150488,1612,403),
    (23,0.0180587,2052,513),
    (24,0.0210693,2732,683),
    (25,0.0260852,3596,899),
    (26,0.035115,4664,1166),
    (27,0.0431422,6092,1523),
    (28,0.0561862,7676,1919),
    (29,0.0732427,9764,2441),
    (30,0.0943121,12176,3044),
    (31,0.1244111,15056,3764),
    (32,0.1655478,18432,4608),
    (33,0.2177204,22656,5664),
    (34,0.2698945,27468,6867),
    (35,0.3351111,33572,8393),
    (36,0.4183867,40712,10178),
    (37,0.5267444,49084,12271),
    (38,0.6401216,58916,14729),
    (39,0.7946332,69648,17412),
    (40,0.9963009,82424,20606),
    (41,1.2260631,95796,23949),
    (42,1.5270608,112000,28000),
    (43,1.8621722,128584,32146),
    (44,2.3076478,148896,37224),
    (45,2.8364009,169696,42424),
    (46,3.5006029,194716,48679),
    (47,4.2842008,220700,55175),
    (48,5.237361,251184,62796),
    (49,6.5186063,282496,70624),
    (50,7.9282791,319368,79842),
    (51,9.8737285,355728,88932),
    (52,12.00981,399248,99812),
    (53,15.0468756,441444,110361),
    (54,18.0267528,491388,122847),
    (55,21.7972514,541204,135301),
    (56,25.9210401,599788,149947),
    (57,31.4567942,658640,164660),
    (58,37.5349419,726092,181523),
    (59,44.9344693,795436,198859),
    (60,54.2505275,871244,217811),
    (61,64.9851091,951360,237840),
    (62,78.3308711,1036500,259125),
    (63,95.4085591,1128628,282157),
    (64,113.8433263,1223804,305951),
    (65,134.8014301,1330572,332643),
    (66,159.0753214,1436068,359017),
    (67,192.2864079,1558792,389698),
    (68,224.4281556,1673608,418402),
    (69,267.9835315,1810440,452610),
    (70,315.5046078,1933976,483494),
    (71,374.7298804,2085192,521298),
    (72,432.8424444,2216500,554125),
    (73,512.1581943,2382196,595549),
    (74,591.3456921,2520988,630247),
    (75,702.4581453,2699488,674872),
    (76,806.0852493,2841288,710322),
    (77,960.4185459,3027856,756964),
    (78,1275.9826106,3166780,791695),
    (79,1678.9187569,3358304,839576),
    (80,2182.9220057,3492660,873165)]
