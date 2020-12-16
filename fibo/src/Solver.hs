module Solver where
import Data.Time.Clock
import Pz8b

------------------------------ ------------------------------
------------------------------ ------------------------------
data Tree a = Node a [Tree a] deriving Show


solver = go aplica
    where
        go f = No (f []) ramos where
            ramos = map g choices
            g m = go (f' m)
            f' m ms = f (m:ms)








idx (No x ts) []     = x
idx (No x ts) (m:ms) = idx (ts !! fromEnum m) ms

ret1 = do 
        t0 <- getCurrentTime
        putStrLn "--------------------------------------"
        print $ idx solver [Cima, Esq]
        putStrLn "--------------------------------------"
        t1 <- getCurrentTime
        print (diffUTCTime t1 t0)
         


         
choices :: [Moves]
choices = [toEnum 0 ..]         
         
       
{-
    go sts = case null goal of
                True -> go sts'
                _    -> goal

       where
         goal = filter (== initSt) sts'
         sts' = [move m s | s <- sts, m <- choices]


       where
         goal = filter (\(s,_) -> s == initSt) sts'
         
         nivel :: [(State, Moves, [Moves])]
         nivel = [(s, m, ms)| (s, ms) <- sts, m <- [toEnum 0 ..]]
         
         sts' =  par nivel
         
         par [] = []
         par ((s, m, ms):xs) = (move m s, m:ms) : par xs

-}

-----------------------------------------------------
{-----------------------------------------------------
testeReduce :: Integer
testeReduce = runPar $ reduce (+) [2..1400]

reduce :: NFData a => (a -> a -> a) -> [a] -> Par a
reduce f [a] = return a
reduce f xs = do
                let (as, bs) = splitAt(length xs `div` 2) xs
                mm <- spawn $ (reduce f bs)
                x <- reduce f as
                y <- get mm 
                
                return (f x y)

-----------------------------------------------------}
-----------------------------------------------------
        








{------------------------------ ------------------------------
data Ilist a = Nil | Cons a (IVar (Ilist a))
type Stream a = IVar (Ilist a)

par :: Stream (State, Moves, [Moves]) -> Par (Stream (State, [Moves]))
par i = do
          o <- new
          fork $ loop i o
          return o

f :: (State, Moves, [Moves]) -> (State, [Moves])
f (s, m, ms) = (move m s, m:ms)  



loop :: Stream [(State, Moves, [Moves])] -> IVar (Ilist [(State, [Moves])]) -> Par ()
loop i o = do
            il <- get i
            case il of 
                Nil -> put o Nil
                Cons x xs -> do
                           nwl <- new
                           put o (Cons (f x) nwl)
                           loop xs nwl




------------------------------ ------------------------------}