module Sokoban where

----------------------------------------------------
----------------------------------------------------
data Moves = Cima | Esq | Dir | Baixo
    deriving (Show, Enum, Bounded)

----------------------------------------------------
----------------------------------------------------
data State = S { x     ::   Int
               , y     ::   Int
               , board :: [String]
               } deriving Eq

instance Show State where
    show (S _ _ b) =  foldr junta "" b
        where 
            junta a b = a ++ "\n" ++ b
                

testSt = S 5 6 testBoard
testBoard = ["#######",
             "#     #",
             "#     #",
             "#. #  #",
             "#. $$ #",
             "#.$$  #",
             "#.#  @#",
             "#######"]
             
             
teste = u (move Cima testSt) 
  where
    u (Just s) = s

----------------------------------------------------
move :: Moves -> State -> Maybe State 
move dir (S x y board) 
    | inBound newX newY = Just (S newX newY newBoard)
    | otherwise = Nothing 

  where 
    newBoard  = swap val board
    val = (board !! newX) !! newY

    pos Cima  = (-1,  0)
    pos Baixo = ( 1,  0)
    pos Esq   = ( 0, -1)
    pos Dir   = ( 0,  1)
    (newX, newY) = addTuple (x, y) (pos dir)
    addTuple (x1, x2) (y1, y2) = (x1+y1, x2+y2)

    swap val board = map (map change) board
      where 
        change sij | sij == val = '@'
                   | sij == '@' = val
                   | otherwise  = sij
                   

    -- inBound x | x >= 0 && x <= 2 = True
    --           | otherwise        = False
    
    inBound x y = True