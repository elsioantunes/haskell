{-# LANGUAGE DeriveFunctor #-}
------------------------------------------
data Writer w a = Writer {runwriter :: (w, a)} deriving (Show, Functor, DeriveAnyClass)
------------------------------------------
instance (Monoid w) => Monad (Writer w) where
    return a = Writer (mempty, a)
    Writer (s1, a) >>= f = 
        let
            (s2, b) = runwriter (f a)
        in
            Writer (s1 <> s2, b)
{------------------------------------------
instance (Monoid w) => Applicative (Writer w) where
    -- pure a = Writer (mempty, a)
    wfw <*> wxw = do
                    f <- wfw
                    fmap f wxw
------------------------------------------}

type FuncWriter = Int -> Writer String Int

negateWriter :: FuncWriter
negateWriter n = Writer ("neguei, ", negate n)

absWriter :: FuncWriter
absWriter n = Writer ("tirei o sinal, ", abs n)

dobraWriter :: FuncWriter
dobraWriter n = Writer ("dobrei", 2 * n)

composiWriter :: FuncWriter
composiWriter n = do
    n1 <- absWriter n
    n2 <- negateWriter n1
    dobraWriter n2
    
main :: IO ()
main = print (composiWriter (-6))

-- Writer {runwriter = ("tirei o sinal, neguei, dobrei",-12)}


{----------------------------------------------------

composiWriter :: FuncWriter  
composiWriter = absWriter >=> negateWriter >=> dobraWriter 

(>=>) :: FuncWriter -> FuncWriter -> FuncWriter 
f1 >=> f2 = \n ->
              let 
                  (s1, n') = f1 n
                  (s2, n'') = f2 n'
              in 
                  (s1 ++ s2, n'')


----------------------------------------------------}

-- ("tirei o sinal, neguei, ",-6)

{-
type Writer w a = (w, a)
type FuncW = Int -> Writer String Int

negateW, dobraW, absW, pegaW, retW, endlW :: FuncW
negateW n = ("neguei, ", negate n)
dobraW  n = ("dobrei, ", 2 * n)
absW    n = ("tirei o sinal, ", abs n)
pegaW   n = ("recebi " ++ show n ++ ", ", n)
retW    n = ("retornei " ++ show n, n)
endlW   n = ("\n", n)

composiW :: FuncW  
composiW = pegaW >=> absW >=> negateW >=> dobraW >=> retW >=> endlW

(>=>) :: FuncW -> FuncW -> FuncW 
f1 >=> f2 = \n ->
              let 
                  (s1, n') = f1 n
                  (s2, n'') = f2 n'
              in 
                  (s1 ++ s2, n'')

main :: IO ()
main = putStr (concatMap (fst . composiW) [1,-6,3,-9,2,-7])




    recebi 1, tirei o sinal, neguei, dobrei, retornei -2
    recebi -6, tirei o sinal, neguei, dobrei, retornei -12
    recebi 3, tirei o sinal, neguei, dobrei, retornei -6
    recebi -9, tirei o sinal, neguei, dobrei, retornei -18
    recebi 2, tirei o sinal, neguei, dobrei, retornei -4
    recebi -7, tirei o sinal, neguei, dobrei, retornei -14
-}










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






