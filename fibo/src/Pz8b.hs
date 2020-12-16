module Pz8b where

------------------------------ ------------------------------
------------------------------ ------------------------------
data Moves = Cima | Baixo | Esq | Dir deriving (Show, Enum)
data Arv s = No s [Arv s] deriving Show
data State = S {x     ::   Int,
                y     ::   Int,
                board :: [[Int]]} deriving Eq




initSt :: State
initSt = S 2 2 initBoard
initBoard = [[1, 2, 3], 
             [4, 5, 6], 
             [7, 8, 0]]

testSt = S 0 1 testBoard
testBoard = [[2, 6, 5], 
             [0, 1, 3], 
             [4, 7, 8]]


arvore :: ([Moves] -> State) -> Arv State
arvore f = No (f []) ramos where
    ramos = map ramifica [toEnum 0 ..]
    ramifica m = arvore (f' m)
    f' m ms = f (m:ms)


indexa :: Arv State -> [Moves] -> State
indexa (No x ts) []     = x
indexa (No x ts) (m:ms) = indexa (ts !! fromEnum m) ms



arv = arvore resolv
teste = print $ indexa arv [Baixo,Cima,Dir,Baixo,Esq,Cima,Esq,Baixo,Baixo,Dir,Dir]


resolv :: [Moves] -> State 
resolv ms = (foldl f id ms) testSt
    where
        f rec x = move x . rec
        
aplica :: [Moves] -> State 
aplica ms = (foldl f id ms) initSt
    where
        f rec x = move x . rec
        
move :: Moves -> State -> State 
move dir (S x y b) = (S newX newY b')
  where 
    b'  = swap val b
    val | inBound newX && inBound newY = (b !! newX) !! newY
        | otherwise                    = 0         
    (newX, newY) = addTuple (x, y) (pos dir)

    pos Cima  = (-1,  0)
    pos Baixo = ( 1,  0)
    pos Esq   = ( 0, -1)
    pos Dir   = ( 0,  1)

    addTuple (x1, x2) (y1, y2) = (x1+y1, x2+y2)

    swap val = map (map change)
      where change sij | sij == val = 0 
                       | sij == 0   = val
                       | otherwise  = sij

    inBound x | x >= 0 && x <= 2 = True
              | otherwise        = False


instance Show State where
    show (S _ _ m ) = "\n" ++ foldr f ("\n ") m 
        where
            f a rec = lin a ++ rec
            lin l = "   |" ++ go l
            go [] = "|\n"
            go (0:xs) = "   " ++ go xs
            go (x:xs) = "[" ++ show x ++ "]" ++ go xs


