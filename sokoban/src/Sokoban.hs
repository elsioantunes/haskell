module Sokoban where

----------------------------------------------------
data Moves = Cima | Esq | Dir | Baixo deriving (Enum, Eq, Bounded, Ord)
----------------------------------------------------

instance Show Moves where
    show Cima  = "u"
    show Baixo = "d"
    show Esq   = "l"
    show Dir   = "r"


----------------------------------------------------
data MovType a = Invalid | Factible a | Goal a deriving Show
----------------------------------------------------
funcSucess :: (Moves, State) -> MovType State
funcSucess (pa, pb) = (go pa pb) where 
  go dir' (S p b d w psh pat dir)
        | isgoal (S p b' d w psh [] dir) = Goal (S p' b' d w psh' pat' dir')  -- A
        | bounds = Invalid 
        | boxfix = Invalid
        | otherwise = Factible (S p' b' d w psh' pat' dir')
  
   where
    p'   = addT p  (pos dir')
    p''  = addT p' (pos dir')
    pat' = dir':pat
    psh' = find p' b

    bounds = find p' w
    boxfix = psh' && (find p'' b || find p'' w)
    -- voltar = (not psh) && (dir' == volta dir) 
    
    b' = push b
    push [] = []
    push (x:xs) | x == p' = p'' : xs 
              | otherwise = x : push xs 

    pos Cima  = ( 0, -1)
    pos Baixo = ( 0,  1)
    pos Esq   = (-1,  0)
    pos Dir   = ( 1,  0)
    
    addT (a, b) (c, d) = (a+c, b+d)

isgoal :: State -> Bool -- estado terminal
isgoal = go . box  where
    go b = calc b == calc (dots testSt) -- A

find :: (Foldable t, Eq a) => a -> t a -> Bool
find = any . (==)

calc :: [(Int, Int)] -> (Int, Int)
calc x = (soma x, prod x)

soma :: [(Int, Int)] -> Int
soma xs = foldl (\acc (a, b) -> acc +  a*30+b)  0 xs

prod :: [(Int, Int)] -> Int
prod xs = foldl (\acc (a, b) -> acc * (a*30+b)) 1 xs

-- volta :: Moves -> Moves
-- volta x = toEnum ((fromEnum (maxBound :: Moves)) - fromEnum x)

----------------------------------------------------
data State = S { player :: (Int, Int)
               , box    :: [(Int, Int)]
               , dots   :: [(Int, Int)]
               , walls  :: [(Int, Int)]
               , push   :: Bool
               , pat    :: [Moves]
               , dir    :: Moves
               } deriving (Eq, Ord)
----------------------------------------------------

toState :: [String] -> State
toState board = S p b d w False [] Dir where -- A
    [[p], b, d, w] = map proc ['@', '$', '.', '#'] 
    proc e = [(x, y) | 
        (y, s) <- zip [0..] board, 
        (x, c) <- zip [0..] s, e == c]
    
eOrd :: State -> ((Int, Int), (Int, Int))
eOrd st = (player st, calc (box st))

{----------------------------------------------------
instance Ord State where
    a <= b = (player a, box a) <= (player b, box b)
----------------------------------------------------}
    
instance Show State where
    show (S p b d w _ pat _) = foldl func1 "\n\t" (z testBoard)  ++ "\n" ++ reverse (concatMap show pat) where -- A
       func1 buf (y, lin) = buf ++ "\n\t" ++  map (limpa y) (z lin)
       limpa y (x, c) | (x, y) == p = '@'
                      | find (x,y) b = '$'
                      | c == '$' || c == '@' = ' '
                      | otherwise = c
       z = zip [0..] 


-- class Solvable where

----------------------------------------------------
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
-- 1.53 case uncurry funcSucess x of
-- 0.03 paralelo ((Nothing, stSet') -> waitLoop (merge stSet stSet') xss )
-- ulullulddurrrddlulrrull 


----------------------------------------------------}





----------------------------------------------------
testSt = toState testBoard
testBoard = [ "     #### ",
              "    ##. ##",
              "##### .  #",
              "#   #  # #",
              "# $ #  # #",
              "# $  @   #",
              "######  ##",
              "     #### "]



-- 16 'pushes' segundo rosetta 
-- 53 lances
-- 0.04s paralelo ((Nothing, stSet') -> waitLoop (merge stSet stSet') xss )
-- lluullddrrrruuurrrdddldlullluuldldrrrrruuurrdddldluuu
{----------------------------------------------------}




{----------------------------------------------------
testSt = toState testBoard
testBoard = [ "#######",
              "#     #",
              "#     #",
              "#. #  #",
              "#. $$ #",
              "#.$$  #",
              "#.#  @#",
              "#######"]

-- primeira versão levou 15 horas (mas resolveu kkk)
-- 30s sokoRosetta
-- 17s loopIO (sem pat2st)
-- 5s  eOrd st = (player st, box st)
-- 2s serial,   eOrd st = (player st, calc (box st))
-- 2s paralelo, look <- lookupMyVar set (eOrd cm) 
----------------------------------------------------}


  

{----------------------------------------------------
testSt = toState testBoard
testBoard = [ "#############",
              "#  #        #",
              "# $$$$$$$  @#",
              "#.......    #",
              "#############"]

-- 30 lances segundo rosetta 
-- ainda sem sol (uniq) 
----------------------------------------------------}




{----------------------------------------------------
testSt = toState testBoard
testBoard = [ "#############",
              "#  #        #",
              "#  $$$$$@   #",
              "#.......$ $ #",
              "#############"]

-- 76 lances?  lldlllllllluurDldRRRRRRRRuulD rdLLLLLLrrrrrurrrdLLLLLLLrrrruulDulDulDulDLLulD segundo rosettacode
-- ainda sem sol (uniq) 
----------------------------------------------------}


{---------------------------------------------------- LEVEL01
testSt = toState testBoard
testBoard = [ 
    "    #####          ",
    "    #   #          ",
    "    #$  #          ",
    "  ###  $##         ",
    "  #  $ $ #         ",
    "### # ## #   ######",
    "#   # ## #####  ..#",
    "# $  $          ..#",
    "##### ### #@##  ..#",
    "    #     #########",
    "    #######        "]

-- ainda sem sol (mesmo em paralelo)

----------------------------------------------------}

  




{----------------------------------------------------
    https://pt.wikipedia.org/wiki/PSPACE-completude    
    
    http://professor.ufabc.edu.br/~e.francesquini/2018.q3.pp/projeto02/
----------------------------------------------------}    







