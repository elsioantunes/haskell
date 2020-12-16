module Main where
-- import Control.Concurrent
import Control.Monad.Par 
import Control.DeepSeq (NFData, rnf)
import Control.Exception
import Data.Time.Clock

{----------------------------------------------------------------
    Streams? 
    https://hackage.haskell.org/package/hinze-streams-1.0/src/Data/Stream/Hinze/Stream.hs
    
    https://hackage.haskell.org/package/Stream-0.4.7.2/docs/src/Data-Stream.html

    
    streamFromList, streamFold, streamMap
   
    https://github.com/simonmar/parconc-examples/blob/master/Stream.hs
    
    https://stackoverflow.com/questions/24773130/parallel-haskell-rate-limiting-the-producer
    
    https://gist.github.com/raymondtay/e7c58b707ac002c3ebb281e7ee577d32
    
    https://github.com/simonmar/monad-par/blob/master/monad-par/Control/Monad/Par/Stream.hs
    
    
    
    documentação dos stats do ghc
    https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime_control.html#rts-options-to-produce-runtime-statistics

----------------------------------------------------------------}
type Stream a = IVar (IList a)

data IList a
  = Nil
  | Cons a (Stream a)
  | Fork (Par ()) (IList a)

instance NFData a => NFData (IList a) where
  rnf Nil = ()
  rnf (Cons a b) = rnf a `seq` rnf b
  rnf (Fork a b) = a `seq` rnf b

----------------------------------------------------------------
main = do
         t0      <- getCurrentTime
         -- tempos  <- mapM (work t0) [60 -x + 6 | x <-[6..60]]
         tempos  <- mapM (work t0)  [48,48,49, 8,40,50, 10,41,60, 7]
         grafico tempos


--
work t0 n = do       
             let fn = n
             t1    <- getCurrentTime
             
             -- teste <- evaluate $ sum ([1 .. 1000000] :: [Double]) -- sequ
             virgula1 <- evaluate $ runPar $ fibo 8
             teste1 <- evaluate $ pipeline func1 fn [1 .. 10000] -- par
             
             virgula2 <- evaluate $ runPar $ fibo 9
             teste2 <- evaluate $ pipeline func2 fn [1 .. 10000] -- par
             
             virgula3 <- evaluate $ runPar $ fibo 10
             
             t2    <- getCurrentTime
             ta    <- evaluate $ nominalDiffTimeToSeconds $ diffUTCTime t2 t0
             tb    <- evaluate $ nominalDiffTimeToSeconds $ diffUTCTime t2 t1
             print (n, tb, ta, teste1, teste2, fn, virgula1, virgula2, virgula3)
             return (n, tb)
--


{-
work t0 n = do       
             let fn = n*10+1
             t1    <- getCurrentTime
             m <- newEmptyMVar
             
             -- teste <- evaluate $ sum ([1 .. 1000000] :: [Double]) -- sequ
             
             forkIO $ do
                     putMVar m (runPar $ fibo 30)
                     putMVar m (pipeline func1 fn [1 .. 1000000])

                     putMVar m (runPar $ fibo 31)
                     putMVar m (pipeline func2 fn [1 .. 1000000])

                     putMVar m (runPar $ fibo 29)

             t2    <- getCurrentTime
             
             virgula1 <- takeMVar m
             teste1 <- takeMVar m
             
             virgula2 <- takeMVar m
             teste2 <- takeMVar m
             
             virgula3 <- takeMVar m
             

             ta    <- evaluate $ nominalDiffTimeToSeconds $ diffUTCTime t2 t0
             tb    <- evaluate $ nominalDiffTimeToSeconds $ diffUTCTime t2 t1
             print (n, tb, ta, teste1, teste2, fn, virgula1, virgula2, virgula3)
             return tb

-}

    where
        func2 a b
                 | even $ round $ a + b = sum [(a+b)/2, (a+b)/3]
                 | otherwise = sum [(a+b)/3, (a+b)/2]
                 
        func1  = (+)
             





---------------------------------------------
pipeline :: (Double -> Double -> Double) -> Int -> [Double] -> Double
pipeline func n list = runPar $ do
        strm <- strmFrList list
        -- strm <- streamFromList list n n
        
        xs   <- streamFold func 0 strm
        return (xs)
    
    



----------------------------------------------------------------
streamFold :: (a -> b -> a) -> a -> Stream b -> Par a
streamFold f acc strm = acc `seq` do
    
    
    ilst <- get strm
    
    case ilst of
        Cons h t          -> streamFold f (f acc h) t
        
        -- aqui o consumidor alcança o produtor
        Fork p (Cons h t) -> fork p >> streamFold f (f acc h) t

        _                 -> return acc


-- k, i : fork distance
-- n : chunk size
------------------------------
streamFromList :: (NFData a, Eq p, Num p) => 
                    [a] -> p -> p -> Par (Stream a)
                    
streamFromList mm k n = do
            var <- new
            fork (loop mm var k)
            return var
    where

        loop [] var _ = put var Nil

        -- (2) aqui o produtor deixa um fork na stream 
        -- que contém a computação para produzir os próximos n valores 
        loop (x:xs) var 0 = do
            tail <- new
            let op = loop xs tail n
            put var (Fork op (Cons x tail))

        -- (1) o produtor vai colocando n computações 
        loop (x:xs) var i = do
            tail <- new
            put var (Cons x tail)
            loop xs tail (i - 1) 
    

----------------------------------------------------------------
-- fromList :: [a] -> Stream a
-- fromList (x:xs) = Cons x (fromList xs)


         
                
strmFrList :: NFData a => [a] -> Par (Stream a)
strmFrList xs = do
        var <- new
        fork (loop xs var)
        return var                      
 where
    loop [] var = put var Nil 
    loop (x:xs) var = do
            tail <- new
            put var (Cons x tail)
            loop xs tail
                
                

strmFrListDo n = do
    str <- strmFrList n
    return str
    
    



                
                
                
                

grafico t = foldMap f t where
    f (n, x) = putStrLn $ bar x n
    g x  = round $ 80 * x / maxx
    maxx = foldr (\(_, a) -> max a) 0 t
    bar x n = show n ++ ":\t " ++ replicate (g x) '#'




    
{-

    head :: Stream a -> a
    head (Cons x _ ) = x
    
    tail :: Stream a -> Stream a
    tail (Cons _ xs) = xs

    inits :: Stream a -> Stream ([a])
    inits xs = Cons [] (fmap (head xs :) (inits (tail xs)))

    tails :: Stream a -> Stream (Stream a)
    tails xs = Cons xs (tails (tail xs))

    map :: (a -> b) -> Stream a -> Stream b
    map f ~(Cons x xs) = Cons (f x) (map f xs)
    
    repeat :: a -> Stream a
    repeat x = Cons x (repeat x)

    iterate :: (a -> a) -> a -> Stream a
    iterate f x = Cons x (iterate f (f x))
    
    -- intersperse 0 [1,2,3,4] = [1,0,2,0,3,0,4]
    intersperse :: a -> Stream a -> Stream a
    intersperse y ~(Cons x xs) = Cons x (Cons y (intersperse y xs))
    
    -- [x1,x2,...] `interleave` [y1,y2,...] == [x1,y1,x2,y2,...]
    interleave :: Stream a -> Stream a -> Stream a
    interleave ~(Cons x xs) ys = Cons x (interleave ys xs)
    
    unfold :: (c -> (a,c)) -> c -> Stream a
    unfold f c =
      let (x,d) = f c
      in Cons x (unfold f d)
      
    splitAt :: Int -> Stream a -> ([a], Stream a)
    splitAt n xs
      | n == 0    = ([],xs)
      | n > 0     = let (prefix,rest) = splitAt (n-1) (tail xs)
                     in (head xs : prefix, rest)
    
    partition :: (a -> Bool) -> Stream a -> (Stream a, Stream a)
    partition p ~(Cons x xs) =
      let (trues,falses) = partition p xs
      in 
        if   p x 
        then (Cons x trues, falses)
        else (trues,        Cons x falses)
    
-}    
    
    
    

{-
    código copiado de
    https://stackoverflow.com/questions/24773130/parallel-haskell-rate-limiting-the-producer

    abstract-par      > using precompiled package
    cereal            > using precompiled package
    data-default-class> using precompiled package
    parallel          > using precompiled package
    primitive         > using precompiled package
    random            > using precompiled package
    vector            > using precompiled package
    monad-par-extras  > using precompiled package
    abstract-deque    > using precompiled package
    math-functions    > using precompiled package
    mwc-random        > using precompiled package
    monad-par         > using precompiled package
    pipepar           > configure (exe)
    Configuring pipepar-0.1.0.0...
    pipepar           > build (exe)
    Preprocessing executable 'pipepar' for pipepar-0.1.0.0..
    Building executable 'pipepar' for pipepar-0.1.0.0..
    [1 of 1] Compiling Main [flags changed]
    Linking .stack-work\dist\29cc6475\build\pipepar\pipepar.exe ...
    pipepar           > copy/register
    Installing executable pipepar in D:\salvador\(facu)\(paradigmas)\Uri\projects\paralelo\pipepar\.stack-work\install\fd6cfb96\bin
    Completed 13 action(s).
-}

-----------------------------------------------------
fibo :: Double -> Par Double
fibo n
      | n < 2 = return 1
      | otherwise = do
                      mm <- spawn(fibo (n-1))
                      a <- fibo (n-2)
                      b <- get mm
                      return (a + b)
-----------------------------------------------------


  
