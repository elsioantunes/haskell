module Sokoban where

----------------------------------------------------
data Moves = Cima | Esq | Dir | Baixo 
    deriving (Enum, Eq, Bounded, Ord)
----------------------------------------------------
instance Show Moves where
    show Cima  = "u"
    show Baixo = "d"
    show Esq   = "l"
    show Dir   = "r"


-------------------------------------------------------------
data MovType a = Invalid | Factible a | Goal a deriving Show
-------------------------------------------------------------
funcSucess :: (Moves, State) -> MovType State
funcSucess (pa, pb) = (go pa pb) where 
  go dir' (S p b d w psh pat dir)
        | isgoal (S p b' d w psh [] dir) = Goal (S p' b' d w psh' pat' dir') 
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
    b' = push b
    push [] = []
    push (x:xs) | x == p' = p'' : xs 
              | otherwise = x : push xs 
    pos Cima  = ( 0, -1)
    pos Baixo = ( 0,  1)
    pos Esq   = (-1,  0)
    pos Dir   = ( 1,  0)
    addT (a, b) (c, d) = (a+c, b+d)


isgoal :: State -> Bool
isgoal = go . box  where
    go b = calc b == calc (dots testSt) 

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
    

type StateMin = ((Int, Int), (Int, Int))

eOrd :: State -> StateMin 
eOrd st = (player st, calc (box st))


data Stt = Stt { getstatemin :: StateMin
               , getiter :: Int
               }

{----------------------------------------------------
instance Ord State where
    a <= b = (player a, box a) <= (player b, box b)
----------------------------------------------------}
    
instance Show State where
    show (S p b d w _ pat _) = foldl func1 "\n\t" (z testBoard)  ++ "\n" ++ reverse (concatMap show pat)  ++ " " ++ show (length pat) where -- A
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





{----------------------------------------------------
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
----------------------------------------------------}




{----------------------------------------------------
testSt = toState testBoard
testBoard = [ "#######",
              "#     #",
              "#. #  #",
              "#. $$ #",
              "#.$$  #",
              "#.#  @#",
              "#######"]
-- luulrddlululdrrrulldlurrrruullldurrddrdlllrul - 45 mvs
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

-- luullulddurrrddlulrrullrrurullluld 34 mvs (record: 1.811s serial 2.4s async)
-- primeira versão levou 15 horas (mas resolveu kkk)
-- 30s sokoRosetta
-- 17s loopIO (sem pat2st)
-- 5s  eOrd st = (player st, box st)
-- 2s serial,   eOrd st = (player st, calc (box st))
-- 2s paralelo, look <- lookupMyVar set (eOrd cm) 
{----------------------------------------------------}



{----------------------------------------------------
testSt = toState testBoard
testBoard = [ "#########",
              "#       #",
              "#       #",
              "# . #   #",
              "# . $$  #",
              "# .$$   #",
              "# .#   @#",
              "#########"]
              
-- ullullulddurrrddlulrrullrrurullluld 35 mvs 44.663s

----------------------------------------------------}


  

{----------------------------------------------------
testSt = toState testBoard
testBoard = [ "##########",
              "#  #     #",
              "# $$$$  @#",
              "#....    #",
              "##########"]

-- alterado daquele do rosetta
-- lldllllluurdldrrrrruuldrdlllrrurrrdllllruuldlluld 49, 6.7694393s 

----------------------------------------------------}



{----------------------------------------------------
testSt = toState testBoard
testBoard = [ "#############",
              "#  #        #",
              "# $$$$$$$  @#",
              "#.......    #",
              "#############"]

-- 30 pushes segundo rosetta 
-- ainda sem sol (uniq) (mais de 52 lances em uma hora, sem resposta)
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
-- 80 lances 40m e nada de resposta 

(1,0s,4,1)
(2,0.0010022s,152,38)
(3,0.0020056s,156,39)
(4,0.0020056s,156,39)
(5,0.0020056s,160,40)
(6,0.0030094s,172,43)
(7,0.0030094s,172,43)
(8,0.0040125s,172,43)
(9,0.0040125s,180,45)
(10,0.0040125s,176,44)
(11,0.0040125s,192,48)
(12,0.0050153s,212,53)
(13,0.0050153s,240,60)
(14,0.0060194s,280,70)
(15,0.0060194s,344,86)
(16,0.0070223s,444,111)
(17,0.0080254s,556,139)
(18,0.009029s,696,174)
(19,0.0100327s,808,202)
(20,0.0110354s,1044,261)
(21,0.0130422s,1200,300)
(22,0.0150488s,1612,403)
(23,0.0180587s,2052,513)
(24,0.0210693s,2732,683)
(25,0.0260852s,3596,899)
(26,0.035115s,4664,1166)
(27,0.0431422s,6092,1523)
(28,0.0561862s,7676,1919)
(29,0.0732427s,9764,2441)
(30,0.0943121s,12176,3044)
(31,0.1244111s,15056,3764)
(32,0.1655478s,18432,4608)
(33,0.2177204s,22656,5664)
(34,0.2698945s,27468,6867)
(35,0.3351111s,33572,8393)
(36,0.4183867s,40712,10178)
(37,0.5267444s,49084,12271)
(38,0.6401216s,58916,14729)
(39,0.7946332s,69648,17412)
(40,0.9963009s,82424,20606)
(41,1.2260631s,95796,23949)
(42,1.5270608s,112000,28000)
(43,1.8621722s,128584,32146)
(44,2.3076478s,148896,37224)
(45,2.8364009s,169696,42424)
(46,3.5006029s,194716,48679)
(47,4.2842008s,220700,55175)
(48,5.237361s,251184,62796)
(49,6.5186063s,282496,70624)
(50,7.9282791s,319368,79842)
(51,9.8737285s,355728,88932)
(52,12.00981s,399248,99812)
(53,15.0468756s,441444,110361)
(54,18.0267528s,491388,122847)
(55,21.7972514s,541204,135301)
(56,25.9210401s,599788,149947)
(57,31.4567942s,658640,164660)
(58,37.5349419s,726092,181523)
(59,44.9344693s,795436,198859)
(60,54.2505275s,871244,217811)
(61,64.9851091s,951360,237840)
(62,78.3308711s,1036500,259125)
(63,95.4085591s,1128628,282157)
(64,113.8433263s,1223804,305951)
(65,134.8014301s,1330572,332643)
(66,159.0753214s,1436068,359017)
(67,192.2864079s,1558792,389698)
(68,224.4281556s,1673608,418402)
(69,267.9835315s,1810440,452610)
(70,315.5046078s,1933976,483494)
(71,374.7298804s,2085192,521298)
(72,432.8424444s,2216500,554125)
(73,512.1581943s,2382196,595549)
(74,591.3456921s,2520988,630247)
(75,702.4581453s,2699488,674872)
(76,806.0852493s,2841288,710322)
(77,960.4185459s,3027856,756964)
(78,1275.9826106s,3166780,791695)
(79,1678.9187569s,3358304,839576)
(80,2182.9220057s,3492660,873165)

----------------------------------------------------}

  




{----------------------------------------------------
    https://pt.wikipedia.org/wiki/PSPACE-completude    
    
    http://professor.ufabc.edu.br/~e.francesquini/2018.q3.pp/projeto02/
----------------------------------------------------}    







