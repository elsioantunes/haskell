--------------------------------------------------
--------------------------------------------------

type State = [Int]
nextfibo :: State -> (Int, State)
nextfibo [a, b] = (a+b, [b, a+b])

devolve :: State -> (Int, State)
devolve [a, b] = (a, [a, b])

nextfiboS :: ST State Int
nextfiboS = sup nextfibo

devolveS :: ST State Int
devolveS  = sup nextfibo

{----------------------------------------------------
fibos = do
           nextfiboS
           nextfiboS
           nextfiboS
           nextfiboS
----------------------------------------------------}
-- replica :: Int -> ST (Int, Int) Int
replica = go where
    go n | n < 2 = devolveS
         | otherwise = do
               nextfiboS
               go (n-1)
               
               
-------------------------
testeNextfibo x = runstate (replica x) [0, 1]
main = print (testeNextfibo 10)
-------------------------





--------------------------------------------
--------------------------------------------
-- Transforma uma func normal em uma ST f --
sup :: (e -> (a, e)) -> ST e a
sup f = S f
--------------------------------------------
newtype ST e a = S {runstate :: (e -> (a, e))}
--------------------------------------------
instance Monad (ST e) where
   S g >>= h = S (\e ->
                    let
                       (x, e') = g e
                       S f = h x
                    in  
                       f e')
--------------------------------------------
instance Applicative (ST e)  where
    pure a = S (\e -> (a, e))
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










change3 :: Num a => a -> ST e a
change3 b = S (\m -> (2*b, m))


doit :: Num b => b -> ST e b
doit a = do 
           -- change1
           -- change2
           b <- change3 a
           return b


newSt = S (\m -> ((), m))

------------------------------------------------
ret1 (a, b) = b

-- testfb1 = ret1 $ (runstate doit) [0..8]
------------------------------------------------
-- main = do
        -- print testfb1
------------------------------------------------
------------------------------------------------


-- teste :: a -> ST e a
--  teste a = return a



--------------------------------------------------------------------------
-- exemplo fibo
--------------------------------------------------------------------------

{-
testeFibo :: [Int]
testeFibo = take 13 infFibos

infFibos :: [Int]
infFibos = 0 : 1 : (fst runFibos)


runFibos :: ([Int], [Int])
runFibos = runstate stFibos [0, 1]

stFibos :: ST [Int] [Int]
stFibos = do
            return [1]


-}
