module Sokoban where
----------------------------------------------------
----------------------------------------------------
data Moves = Cima | Esq | Dir | Baixo
    deriving (Enum, Eq, Bounded)

instance Show Moves where
    show Cima  = "u"
    show Baixo = "d"
    show Esq   = "l"
    show Dir   = "r"

----------------------------------------------------
data State = S { player :: (Int, Int)
               , box    :: [(Int, Int)]
               , dots   :: [(Int, Int)]
               , walls  :: [(Int, Int)]
               , psh    :: Bool
               } deriving Eq


instance Ord State where
    a <= b = ordSt a <= ordSt b
    
instance Show State where
    show (S p b d w _) = foldl func1 "\n\t" (z testBoard) where
    -- show (S p b d w) = foldl func1 "\n\t" (z testBoard) where
       func1 buf (y, lin) = buf ++ "\n\t" ++  map (limpa y) (z lin)
       limpa y (x, c) | (x, y) == p = '@'
                      | elem (x,y) b = '$'
                      | c == '$' || c == '@' = ' '
                      | otherwise = c
       z = zip [0..] 


type A = (Bool, (Int, Int), [(Int, Int)])
-- type A = ((Int, Int), [(Int, Int)])
ordSt :: State -> A
ordSt x = (psh x, player x, box x)
-- ordSt x = (player x, box x)

toState :: [String] -> State
toState board = S p b d w False where
-- toState board = S p b d w where
    [[p], b, d, w] = map proc ['@', '$', '.', '#'] 
    proc e = [(x, y) | 
        (y, s) <- zip [0..] board, 
        (x, c) <- zip [0..] s, e == c]
    
-- maxw = go testSt where
-- class Solvable where

----------------------------------------------------
----------------------------------------------------
isgoal :: A -> Bool
isgoal = go where
    go (False, _, _) = False
    go (_, _, b) = calc b == calc (dots testSt)
    -- go (_, b) = calc b == calc (dots testSt)

calc :: [(Int, Int)] -> (Int, Int)
calc x = (soma x, prod x)

soma :: [(Int, Int)] -> Int
soma xs = foldl (\acc (a, b) -> acc +  a*30+b)  0 xs

prod :: [(Int, Int)] -> Int
prod xs = foldl (\acc (a, b) -> acc * (a*30+b)) 1 xs


----------------------------------------------------
funcSucess :: Moves -> A -> Maybe A
funcSucess dir (_, p, b) 
-- funcSucess dir (p, b) 
    | boxfix = Nothing 
    | bounds = Nothing 
    | otherwise = Just (psh, p', b') 
    -- | otherwise = Just (p', b') 

  where 
    w = walls testSt
    p'  = addT p  (pos dir)
    p'' = addT p' (pos dir)

    psh = elem p' b
    bounds = elem p' w
    boxfix = psh && (elem p'' w || elem p'' b)
    
    b' = push b
    push [] = []
    push (x:xs) | x == p' = p'' : xs 
              | otherwise = x : push xs 

    pos Cima  = ( 0, -1)
    pos Baixo = ( 0,  1)
    pos Esq   = (-1,  0)
    pos Dir   = ( 1,  0)
    
    addT (a, b) (c, d) = (a+c, b+d)

----------------------------------------------------




{----------------------------------------------------
testSt = toState testBoard
testBoard = [ "#######",
              "#     #",
              "#     #",
              "#  $  #",
              "# # # #",
              "#  .  #",
              "#  #  #",
              "#  @  #",
              "#######"]

----------------------------------------------------}



{----------------------------------------------------
testSt = toState testBoard
testBoard = [ "#######",
              "#.    #",
              "#$   $#",
              "#    .#",
              "# # # #",
              "#     #",
              "#  #  #",
              "#  @  #",
              "#######"]
 -- [Esq,Cima,Cima,Esq,Cima,Cima,Cima,Dir,Cima,Dir,Dir,Dir,Baixo] 2.604651s
 -- avaliar a legalidade dos movimentos e filtrar repetidos
 -- 0.3180583s concat [maybemove m s | s <- sts, m <- [toEnum 0 ..]] 
 -- 0.3090235s catm ls = [x | Just x <- ls]
 -- 0.0030088s uniq (x:xs) = x : uniq (filter (/=x) xs) 
 ---------------------------------------------------}


{----------------------------------------------------
testSt = toState testBoard
testBoard = [ "#######",
              "#.    #",
              "#$   $#",
              "#    .#",
              "##### #",
              "#     #",
              "#  #  #",
              "#  @  #",
              "#######"]


-- [Dir,Cima,Cima,Dir,Cima,Cima,Esq,Cima,Cima,Dir,Baixo,Esq,Esq,Esq,Baixo,Esq,Cima] 234s
-- 0.0040134s uniq
----------------------------------------------------}




{----------------------------------------------------
testSt = toState testBoard
testBoard = [ "#######",
              "#.    #",
              "#$  $ #",
              "#     #",
              "# #   #",
              "# #   #",
              "#$    #",
              "#. @ .#",
              "#######"]

-- 2.9417324s (uniq)
-- 0.8548583s filterSet m set = filter (go set) ...
-- 0.2473229s updateSet e member
-- 0.1444777s bfs iter' (updateSet set sts') (updateSet Empty sts')
----------------------------------------------------}




----------------------------------------------------
testSt = toState testBoard
testBoard = [ "#######",
              "#     #",
              "#     #",
              "#. #  #",
              "#. $$ #",
              "#.$$  #",
              "#.#  @#",
              "#######"]

-- 30s sokoRosetta
-- 17s loopIO (sem pat2st)

{----------------------------------------------------
    (1,0s,2,2,1)
    (2,0.001002s,3,4,2)
    (3,0.001002s,2,4,3)
    (4,0.001002s,6,7,2)
    (5,0.002006s,13,14,6)
    (6,0.002006s,26,32,13)
    (7,0.002006s,43,54,26)
    (8,0.0040118s,76,96,43)
    (9,0.0050152s,114,152,76)
    (10,0.0090285s,179,237,114)
    (11,0.014045s,247,339,179)
    (12,0.0250817s,368,509,247)
    (13,0.0441447s,508,698,368)
    (14,0.0812689s,731,1036,508)
    (15,0.1514997s,969,1369,731)
    (16,0.2889548s,1365,1973,969)
    (17,0.5387822s,1809,2553,1365)
    (18,1.0294045s,2529,3646,1809)
    (19,1.8581464s,3222,4624,2529)
    (20,3.4082749s,4411,6438,3222)
    (21,6.0690792s,5472,7968,4411)
    (22,11.5050644s,7402,10937,5472)
    (23,20.377416s,8936,13184,7402)
    (24,39.5679073s,11979,17906,8936)
    (25,70.5865331s,14214,21102,11979)
    (26,139.6169152s,18837,28503,14214)
    (27,246.8617299s,22056,33102,18837)
    (28,464.309867s,28994,44229,22056)
    (29,779.2321129s,33259,50444,28994)
    (30,1335.4502334s,43383,66990,33259)
    (31,2133.3534225s,49441,75318,43383)
    (32,3504.2232103s,63382,99416,49441)
    (33,5209.2691577s,70841,109168,63382)
    (33,5577.1881767s,70841,109168,63382)
    (34,5938.1934643s,89294,142392,70841) --  1.65 horas
----------------------------------------------------}



---------------------------------------------------



{----------------------------------------------------
testSt = toState testBoard
testBoard = [ "#######",
              "#     #",
              "#  #  #",
              "#. $  #",
              "#.$$  #",
              "#.#  @#",
              "#######"]

-- 0.1384583s (uniq + filterSet) 23 iter

----------------------------------------------------}




  

{----------------------------------------------------
testSt = toState testBoard
testBoard = [ "#############",
              "#  #        #",
              "#  $$$$$@   #",
              "#.......$ $ #",
              "#############"]

-- ainda sem sol (uniq)
----------------------------------------------------}


{---------------------------------------------------- LEVEL01
    #####
    #   #
    #$  #
  ###  $##
  #  $ $ #
### # ## #   ######
#   # ## #####  ..#
# $  $          ..#
##### ### #@##  ..#
    #     #########
    #######
----------------------------------------------------}

  




{----------------------------------------------------
    
    https://pt.wikipedia.org/wiki/PSPACE-completude    
    
    http://professor.ufabc.edu.br/~e.francesquini/2018.q3.pp/projeto02/
    
----------------------------------------------------}    


{----------------------------------------------------

----------------------------------------------------
pat2st :: [Moves] -> Maybe State
pat2st [] = Just testSt
pat2st (x:xs) = func x (pat2st xs) where
    func x Nothing = Nothing
    func x (Just st) = move x st

----------------------------------------------------
type UnicSok = A
unicSokoban :: State -> UnicSok
-- unicSokoban :: Ord a => State -> a
unicSokoban s = (player s, box s)

----------------------------------------------------}
