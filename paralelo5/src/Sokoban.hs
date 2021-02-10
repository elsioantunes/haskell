module Sokoban where
----------------------------------------------------
----------------------------------------------------
data Moves = Cima | Esq | Dir | Baixo
    deriving (Show, Enum, Eq, Bounded)

data State = S { player :: (Int, Int),
                 box  :: [(Int, Int)], 
                 dots :: [(Int, Int)], 
                 walls :: [(Int, Int)] 
               } deriving Eq

instance Show State where
    show (S p b d w) = foldl func1 "\n\t" (z testBoard) where
       func1 buf (y, lin) = buf ++ "\n\t" ++  map (limpa y) (z lin)
       limpa y (x, c) | (x, y) == p = '@'
                      | elem (x,y) b = '$'
                      | c == '$' || c == '@' = ' '
                      | otherwise = c
       z = zip [0..] 

----------------------------------------------------
----------------------------------------------------
isGoal :: State -> Bool
isGoal (S _ b d _) = (s b, p b) == (s d, p d)
  where
    s xs = foldr addT (0,0) xs
    p xs = foldr mulT (1,1) xs

addT (a, b) (c, d) = (a+c, b+d)
mulT (a, b) (c, d) = (a*c, b*d)

----------------------------------------------------
testSt2 = toState testBoard2
testBoard2 = ["#######",
              "#     #",
              "#     #",
              "#. #  #",
              "#. $$ #",
              "#.$$  #",
              "#.#  @#",
              "#######"]

testSt = toState testBoard
testBoard = [ "#######",
              "#     #",
              "#$    #",
              "#. #  #",
              "#     #",
              "#     #",
              "#@#   #",
              "#######"]

----------------------------------------------------
toState :: [String] -> State
toState board = S p b d w where
    [[p], b, d, w] = map proc ['@', '$', '.', '#'] 
    proc e = [(x, y) | (y, s) <- zip [0..] board,  (x, c) <- zip [0..] s, e == c]

----------------------------------------------------
pat2st :: [Moves] -> Maybe State
pat2st [] = Just testSt
pat2st (x:xs) = func x (pat2st xs) where
    func x Nothing = Nothing
    func x (Just st) = move x st

----------------------------------------------------
move :: Moves -> State -> Maybe State 
move dir (S p b d w) 
    | bounds = Nothing 
    | boxfix = Nothing 
    | otherwise = Just (S p' b' d w)

  where 
    p'  = addT p  (pos dir)
    p'' = addT p' (pos dir)

    bounds = elem p' w
    boxfix = elem p' b && (elem p'' w || elem p'' b)
    
    b' = push b
    push [] = []
    push (x:xs) | x == p' = p'' : xs 
              | otherwise = x : push xs 

    pos Cima  = ( 0, -1)
    pos Baixo = ( 0,  1)
    pos Esq   = (-1,  0)
    pos Dir   = ( 1,  0)
    
    
----------------------------------------------------
teste1 x = do
    maybe testSt id (pat2st x)

teste2 = teste1 $ reverse [
    Cima, Esq, Cima, Esq, Esq, Cima, Esq
    , Baixo, Baixo, Cima, Dir, Dir, Dir
    , Baixo, Baixo, Esq, Cima, Esq, Dir
    , Dir, Cima, Esq, Esq, Dir, Dir
    , Cima, Dir, Cima, Esq, Esq
    , Esq, Cima
    , Esq
    -- , Baixo
    ]


teste1a x = do
    maybe testSt2 id (pat2st x)

teste2a = teste1a $ reverse [
    -- Cima, Esq, Cima, Esq, Esq, Cima, Esq
    -- , Baixo, Baixo, Cima, Dir, Dir, Dir
    -- , Baixo, Baixo, Esq, Cima, Esq, Dir
    -- , Dir, Cima, Esq, Esq, Dir, Dir
    -- , Cima, Dir, Cima, Esq, Esq
    -- , Esq, Cima
    -- , Esq
    Baixo
    ]
{----------------------------------------------------}
    