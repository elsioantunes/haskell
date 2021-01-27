{-# LANGUAGE CPP, MagicHash, UnboxedTuples, BangPatterns #-}
module Solver where

import Pz8c
import Async

import Data.IORef (IORef (..), atomicModifyIORef, newIORef)
import Control.Exception (finally)                
import GHC.Conc (forkIO)
import Control.Concurrent (putMVar, takeMVar, newEmptyMVar, newMVar, modifyMVar, MVar(..))

-- apenas para testar o codigo do diretorio
import Data.List(sort)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

import Data.Time.Clock (getCurrentTime, diffUTCTime)


---------------------------------------------------------------------------











{----------------------------------------------------
    Codigo base do backtraking.
    
    Sequencial e sem mostrar o caminho
    percorre todas as possibilidades em `BFS`
    até encontrar o `initSt` apenas aplicando `move` 
    fornecido no módulo `Pz8b`
    
    trata-se de um map-reduce, que explode os estados 
    depois reduz num processo de folding
    
    sem resposta, mas conclui na media estável: 1.4s
----------------------------------------------------}
solver0 :: State -> State
solver0 st = head (go [st])  where
    go sts | null goal = go sts'
           | otherwise = goal
      where
        goal = filter (== initSt) sts'
        sts' = do
                s <- sts
                m <- [toEnum 0 ..]
                return (move m s)
--------------------------
ret0 :: IO ()
ret0 = do
    print (solver0 testSt1)
--------------------------









{----------------------------------------------------
    Versão que mostra o caminho através de uma lista, 
    inserida localmente vazia no início da função.
    
    A lista se enche com todas as ramificações
    e estoura a memória rapidamente.
    
    Apesar de funcional e rápido, o código não escalável.
    resposta esperada na media estável: 1.6s
    
----------------------------------------------------}
solver0b :: State -> [(State, [Moves])]
solver0b st = go [(st, [])]  where
    go sts | null goal = go sts'
           | otherwise = goal
      where
        goal = filter (\(s, _) -> s == initSt) sts'
        sts' = do
                (s, caminho) <- sts
                m <- [toEnum 0 ..]
                return (move m s, m:caminho)
--------------------------
ret0b :: IO ()
ret0b = do
    let ((s, r):_) = solver0b testSt1
    print s
    print $ reverse r
--------------------------















{----------------------------------------------------
  Versão (*equivocada) com retorno em IO para utilização de async.
  Uma das formas de paralelizar o código
  
  * equivocada pois implementa BFS com lista dos lvls
  
  retorna resposta esperada em 5.4s (média estável)
  bem mais lento que a versão anterior (pura)
  
  overheads: pat2st(?) e lvls
  
  utilização do `pat2st` para não carregar o estado
  
  versão `solver0e` mostra que 
    versão IO não causa overhead por si só 
  
  
----------------------------------------------------}
type Caminho = [Moves]
type Resp = Maybe Caminho
type Dados = ([[Caminho]], [Caminho])

solver4 :: State -> IO Resp
solver4 st = loop ([], [[]])
  where                              
    -- caso base ---------------------------
    resolve :: Caminho -> IO (Either [Caminho] Caminho)
    resolve path | isgoal    = return (Right path) 
                 | otherwise = return (Left sucessors) 
      where
        isgoal = pat2st path == st
        sucessors = map neib [toEnum 0 ..]
        neib m = m:path
 
    -- loop principal recursivo ------------
    loop :: Dados -> IO Resp 
    loop (lvls, [])     = loop ([], concat lvls)
    loop (lvls, (x:xs)) = do
        r <- resolve x
        case r of
            Left sucessors -> loop ((sucessors:lvls), xs)
            Right path     -> return (Just path)
            
--------------------------
ret4 :: IO Resp
ret4 = do 
    solver4 testSt1  
----------------------------------------------------












----------------------------------------------------
-- guardando levels em Mvars's ---------------------

newLevelM :: a -> IO (MVar a)
newLevelM xs = do
    v <- newEmptyMVar
    forkIO (putMVar v xs)
    return v

insLevelM :: MVar [a] -> a -> IO ()
insLevelM v x = modifyMVar v lvl
  where
    lvl xs = return $ let 
                !z = x:xs
             in 
                (z, ())

getLevelsM :: MVar [a] -> IO [a]
getLevelsM v = modifyMVar v lvl
  where
    lvl xs = return $ ([], xs)
----------------------------------------------------










----------------------------------------------------
-- guardando levels em IORef's ---------------------

newLevel :: [a] -> IO (IORef [a])
newLevel xs = do
    v <- newIORef xs
    return v

insLevel :: (IORef [a]) -> a -> IO ()
insLevel v x = atomicModifyIORef v lvl
  where
    lvl xs = let 
                !z = x:xs
             in 
                (z, ())

getLevels :: (IORef [a]) -> IO [a]
getLevels v = atomicModifyIORef v lvl
  where
    lvl xs = ([], xs)
----------------------------------------------------











{----------------------------------------------------
  versão com lista lvls usando IORefs

    type Caminho = [Moves]
    type Resp = Maybe Caminho

----------------------------------------------------}

-- solver4b :: State -> IO Resp
solver4b lv st = (fmap . fmap) inverte (loop [[]])
  where                              
    -- caso base ---------------------------
    resolve :: Caminho -> IO (Either [Caminho] Caminho)
    resolve path | isgoal    = return (Right path) 
                 | otherwise = return (Left sucessors) 
      where
        isgoal = pat2st path == st
        sucessors = map neib [toEnum 0 ..]
        neib m = m:path
 
    -- loop principal recursivo ------------
    loop :: [Caminho] -> IO Resp
    loop [] = do
                lvls <- getLevels lv 
                loop (concat lvls)
                   
    loop (x:xs) = do
        r <- resolve x
        case r of
            Left sucessors -> do 
                               insLevel lv sucessors
                               loop xs
                               
            Right path     -> return (Just path)
            
--------------------------
ret4b :: IO Resp
ret4b = do 
    lv <- newLevel []
    solver4b lv testSt1  
----------------------------------------------------






      
    
{----------------------------------------------------
    Serializando para paralelizar
    Versão que enfilera Jobs em uma lista [Async Job]
    
    type Caminho = [Moves]
    type Resp = Maybe Caminho
----------------------------------------------------}
type Job = (Either [Caminho] Caminho)

solverPar0 :: IORef [[Caminho]] -> State -> IO Resp
solverPar0 lv st = (fmap . fmap) inverte (subLoop [[]] [])
  where                              
    -- caso base ---------------------------
    resolve :: Caminho -> IO Job
    resolve path | isgoal    = return (Right path) 
                 | otherwise = return (Left sucessors) 
      where
        isgoal = pat2st path == st
        sucessors = map neib [toEnum 0 ..]
        neib m = m:path
 
    -- loop principal recursivo ------------
    loop :: [Async Job] -> IO Resp
    loop [] = do
               lvls <- getLevels lv 
               subLoop (concat lvls) []
    
    loop (x:xs) = do
        r <- wait x
        
        case r of
            Right path     -> return (Just path)
            Left sucessors -> do 
                                insLevel lv sucessors
                                loop  xs
    
    ----------------------------------------
    operador :: Caminho -> ([Async Job] -> IO Resp) -> ([Async Job] -> IO Resp)
    operador x inner = \asyncs -> do
        withAsync  (resolve x) $ \a -> do 
            inner (a:asyncs)
        
    -- withAsync: Spawning with automatic cancelation
    ----------------------------------------
    subLoop :: [Caminho] -> ([Async Job] -> IO Resp)
    subLoop xs = foldr operador loop xs

    ----------------------------------------
--------------------------
ret5 :: IO Resp  -- erro no ghci
ret5 = do 
    lv <- newLevel []
    solverPar0 lv testSt1  
    -- return Nothing














----------------------------------------------------
-- Semaforo ----------------------------------------
type Semaf = IORef Int 

newSemaf :: Int -> IO Semaf 
newSemaf i = do
    m <- newIORef i
    return m

obtemTicket :: Semaf -> IO Bool
obtemTicket m = atomicModifyIORef m sem
  where
    sem :: Int ->    (Int, Bool)    
    sem i | i == 0    = (i, False)
          | otherwise = let  
                          !z = i - 1
                        in 
                          (z, True)

liberaSemaf :: Semaf -> IO ()
liberaSemaf m = atomicModifyIORef m sem
  where
    sem :: Int ->    (Int, ())
    sem i  = let
               !z = i + 1 -- ? "avoid building up a large expression inside the IORef: 1 + 1 + 1 + ...." /Simon
             in
                (z, ())

----------------------------------------------------




    
    
    
    
    
    




{----------------------------------------------------
    Versão com semáforos IORef para limitar threads
    
    type Caminho = [Moves]
    type Resp = Maybe Caminho
    type Job = (Either [Caminho] Caminho)
    
----------------------------------------------------}

-- caso base ---------------------------
resolve :: State -> Caminho -> IO Job
resolve st pat = return (go pat)
  where
    go path | isgoal    = Right path 
            | otherwise = Left sucessors
      where
        isgoal = pat2st path == st
        sucessors = map neib [toEnum 0 ..]
        neib m = m:path

----------------------------------------
solverPar2 :: IORef [[Caminho]] -> Semaf -> State -> IO Resp
solverPar2 lv sem st = drive

  where                              
    drive = (fmap . fmap) inverte (subLoop [[]] [])
    
    loop :: [Async Job] -> IO Resp
    loop [] = do
               lvls <- getLevels lv 
               subLoop (concat lvls) []
    
    loop (x:xs) = do
        r <- wait x
        
        case r of
            Right path     -> return (Just path)
            Left sucessors -> do 
                                insLevel lv sucessors
                                loop  xs
    
    ----------------------------------------
    subLoop :: [Caminho] -> ([Async Job] -> IO Resp)
    subLoop xs = foldr operador rloop xs
    
    rloop m = loop (reverse m)

    ----------------------------------------
    operador :: Caminho -> ([Async Job] -> IO Resp) -> ([Async Job] -> IO Resp)
    operador x inner asyncs = do

        ok <- obtemTicket sem

        if ok then do
            let doit = (resolve st x) `finally` (liberaSemaf sem)
            doit `withAsync` (\a -> inner (a:asyncs))
        
        else do
            r <- resolve st x
            case r of
                Right path -> return (Just path)
                Left sucessors -> do 
                                   insLevel lv sucessors
                                   return Nothing
                
    ----------------------------------------

     
    
    
    
--------------------------
ret5b :: IO Resp
ret5b = do 
     
    lv <- newLevel []  -- se usar MVar, não compila mas roda no ghc :(
    sem <- newSemaf 12
    solverPar2 lv sem testSt1
    
    
----------------------------------------------------











----------------------------------------------------
----------------------------------------------------
type A = Maybe FilePath
type B = FilePath

-- find3 :: IORef Int -> String -> B -> IO A
-- find3 semaf str b =  (resolve b)  where                              
find3 :: String -> B -> IO A
find3 str b =  (resolve b)  where                              
    subLoop :: [Async A] -> [B] -> IO A 
    subLoop asyncs []     = loop asyncs
    subLoop asyncs (x:xs) = do
----------------------------------------------------
            withAsync (resolve x) $ \a -> 
                subLoop (a:asyncs) xs
{----------------------------------------------------
        q <- obtemTicket semaf

        if q then do
            let dofind = (resolve x) `finally` (liberaSemaf semaf)
            withAsync dofind $ \a -> 
                subLoop (a:asyncs) xs

        else do 
            r <- resolve x
            case r of  
                Nothing -> subLoop asyncs xs
                Just _  -> return r
----------------------------------------------------}
    loop :: [Async A] -> IO A
    loop [] = return Nothing      
    loop (x:xs) = do
            r <- wait x
            case r of                              
                Nothing -> loop xs
                Just _  -> return r

    resolve :: B -> IO A
    resolve path = do
        dirContent <- getDirectoryContents path 
        let files = sort $ filter (`notElem` [".",".."]) dirContent  

        if any (== str) files then 
            return (Just path)

        else do
            let pathFiles = map (path </>) files
            paths <- filterIO doesDirectoryExist pathFiles
            subLoop [] paths 


--------------------------
ret5c :: IO A
ret5c = do
    -- semaf   <- newSemaf 12
    -- find3 semaf "reduce2c.png" "D:\\salvador\\(facu)\\" 
    find3 "reduce2c.png" "D:\\salvador\\(facu)\\" 
----------------------------------------------------
----------------------------------------------------

















{----------------------------------------------------
    revisão do codigo solvePar2
    resultado:  CRASH! 
    provavelmente por estar se perdendo no DFS
    
    type Caminho = [Moves]
    type Resp = Maybe Caminho

----------------------------------------------------}
solverPar2a :: State -> IO Resp
solverPar2a st = (fmap . fmap) inverte (subLoop [] [[]])
  where                              
    ----------------------------------------
    subLoop :: [Async Resp] -> [Caminho] -> IO Resp
    subLoop asyncs [] = loop asyncs
    subLoop asyncs (x:xs) = do
            withAsync (resolve x) $ \a -> 
                subLoop (a:asyncs) xs
                
    ----------------------------------------
    loop :: [Async Resp] -> IO Resp
    loop [] = return Nothing
    loop (x:xs) = do
        r <- wait x
        case r of
            Nothing -> loop  xs
            Just _  -> return r
    
    -- caso base ---------------------------
    -- resolve pat = return (go pat)
    resolve :: Caminho -> IO Resp 
    resolve path = do
        let dirContent = map (\m -> m:path) [toEnum 0 ..]
            files = map pat2st dirContent
            
        if any (== st) files then
            return (Just path)

        else do
            subLoop [] dirContent
        

--------------------------
ret5d :: IO Resp
ret5d = do 
    solverPar2a testSt1

----------------------------------------------------







{----------------------------------------------------
    revisão do codigo solvePar2a
    
    tranformando DFS em 'Depth Threshold'
    com SEMÁFOROS
    
    resultado: 
        NÃO funcionou. 
        loop infinito (sem crash)
        
    MOTIVO: 
        não sei.
        provavelmente a premissa de que 
        o codigo é um 'Depth Threshold' 
        
    
    type Caminho = [Moves]
    type Resp = Maybe Caminho
    type Job = (Either [Caminho] Caminho)
    
----------------------------------------------------}
solverPar2b :: IORef Int -> State -> IO Resp
solverPar2b semaf st = (fmap . fmap) inverte (subLoop [] [[]])
  where                              
    ----------------------------------------
    subLoop :: [Async Resp] -> [Caminho] -> IO Resp
    subLoop asyncs [] = loop asyncs
    subLoop asyncs (x:xs) = do
{----------------------------------------------------
            withAsync (resolve x) $ \a -> 
                subLoop (a:asyncs) xs
----------------------------------------------------}
            q <- obtemTicket semaf

            if q then do
                let dofind = (resolve x) `finally` (liberaSemaf semaf)
                withAsync dofind $ \a -> 
                    subLoop (a:asyncs) xs

            else do 
                r <- resolve x
                case r of  
                    Nothing -> subLoop asyncs xs
                    Just _  -> return r
                
    ----------------------------------------
    loop :: [Async Resp] -> IO Resp
    loop [] = return Nothing
    loop (x:xs) = do
        r <- wait x
        case r of
            Nothing -> loop  xs
            Just _  -> return r
    
    -- caso base ---------------------------
    -- resolve pat = return (go pat)
    resolve :: Caminho -> IO Resp 
    resolve path = do
        let dirContent = map (\m -> m:path) [toEnum 0 ..]
            files = map pat2st dirContent
            
        if any (== st) files then
            return (Just path)

        else 
            subLoop [] dirContent
        

--------------------------
ret5e :: IO Resp
ret5e = do 
    semaf <- newSemaf 4
    solverPar2b semaf testSt1

----------------------------------------------------

















{----------------------------------------------------
  utilizando `pat2st` para não carregar o estado
  mas cria overheads(*). Economiza memória mas gasta tempo
  
  estado e caminho são REPRESENTAÇÕES da mesma coisa
  então, não deveriam ser transportados juntos.
  
  tempo estável: 3.6s
  
    troca de `filter` por `any`. Jogando filter
    pra fora do codigo principal GERA OVERHEADS
  
  * a versão IO correta `solver0e` mostrou que nem tanto
----------------------------------------------------}
solver0c :: State -> [[Moves]]
solver0c st = go [[]]  where
    go paths | null goal = go paths'
             | otherwise = goal
      where
        goal = filter ((st ==) . pat2st) paths'
        
        paths' = do
                  caminho <- paths
                  m <- [toEnum 0 ..]
                  return (m:caminho)

--------------------------
ret0c :: IO ()
ret0c = do
    let (r:_) = solver0c testSt1
    print $ inverte r
--------------------------






















{----------------------------------------------------
  inacabado 
----------------------------------------------------}
data Tree a = Node (Maybe a) [Tree a] deriving Show

ramify :: Tree Moves -> Tree Moves
ramify = go where
    go (Node m []) = Node m ramif
    go (Node m xs) = Node m (map go xs)
    ramif = map (\m -> Node (Just m) []) [toEnum 0 ..]



{----------------------------------------------------
statefy :: Tree Moves -> [States]
statefy = go where
    go (Node m []) = Node m ramif
    go (Node m xs) = Node m (map go xs)
    ramif = map (\m -> Node m []) [toEnum 0 ..]
----------------------------------------------------}














{----------------------------------------------------
    * VERSÃO IO CORRETA!
    
    nova tentativa de separar o código em
    um retorno de IO e um loop de retorno de IO

    -----------------------------
    o BFS desse código é do tipo:
    -----------------------------
    bfs pats | (not null goal) = head (filter ((st==).pat2st) pats')
             | otherw          = bfs pats'
                               
        onde 
            pats' = [(m:pat) | pat <- paths, m <- [toEnum 0 ..]]
    
    
    -----------------------------
    trocando filter por loop
    -----------------------------
    bfs pats | isgoal = res
             | otherw = bfs pats'
        onde 
        (isgoal, res) = loop ((st==) . pat2st) pats'
        
        loop p = go
            go []           = (False, [])
            go (x:xs) | p x = (True,  x)     <-- tem uma saída aqui
                      | otherwise = go xs    <-- e um loop
    
    -----------------------------
    trocando filter por loop
    -----------------------------
----------------------------------------------------}








{----------------------------------------------------
    Versão BFS IO funcionando perfeitamente
    com tempo estável em 3.8s (ghci) 0.09s (compilado!)
    MESMO SEM paralelização no código
    speedup entre 2.38 e 2.77 *
        * dependendo se a medida for pelo getCurrentTime
        ou pelo log do ghc
    
    -------------------------
    type Caminho = [Moves]
    type Resp = Maybe Caminho
----------------------------------------------------}
solver0e :: State -> IO Resp
solver0e st = (fmap . fmap) inverte (bfs [[]])  
  where
    bfs paths = do
        r <- loopIO paths'
        case r of
            Nothing -> bfs paths'
            Just _  -> return r

      where
        paths' = [(m:pat) | pat <- paths, m <- [toEnum 0 ..]]
        --------------------------
        predic :: Caminho -> IO Bool
        predic x = return (go x)  where
            go = (st ==) . pat2st

        --------------------------
        loopIO :: [Caminho] -> IO Resp
        loopIO = go
          where
            go [] = return Nothing
            go (x:xs) = do
                         r <- predic x
                         case r of 
                            True  -> return (Just x)
                            False -> go xs
    
--------------------------
ret0e :: IO Resp
ret0e = do
    solver0e testSt1
    
--------------------------














{----------------------------------------------------
    
    -------------------------
    type Caminho = [Moves]
    type Resp = Maybe Caminho
----------------------------------------------------}
-- solver0f :: IORef Int -> State -> IO resp
-- solver0f semaf st = (fmap . fmap) inverte (bfs [[]])  
solver0f :: State -> IO Resp
solver0f st = (fmap . fmap) inverte (bfs [[]])  
  where
    bfs paths = do
        -- r <- loopIO paths'
        r <- subloop [] paths'
        case r of
            Nothing -> bfs paths'
            Just _  -> return r

      where
        subloop :: [Async Resp] -> [Caminho] -> IO Resp
        subloop asyncs [] = loopIO asyncs
        subloop asyncs (x:xs) = do
----------------------------------------------------
            withAsync (resolve x) $ \a ->
                subloop (a:asyncs) xs
{----------------------------------------------------
            q <- obtemTicket semaf

            if q then do
                let dofind = (resolve x) `finally` (liberaSemaf semaf)
                withAsync dofind $ \a -> 
                    subloop (a:asyncs) xs

            else do 
                r <- resolve x
                case r of  
                    Nothing -> subloop asyncs xs
                    Just _  -> return r
----------------------------------------------------}

        --------------------------
        paths' :: [Caminho]
        paths' = [(m:pat) | pat <- paths, m <- [toEnum 0 ..]]
        
        --------------------------
        resolve :: Caminho -> IO Resp
        resolve  = go where
            go x | st == pat2st x = return (Just x)
                 | otherwise = return Nothing
          
        --------------------------
        -- loopIO :: [Caminho] -> IO Resp
        loopIO :: [Async Resp] -> IO Resp
        loopIO = go
          where
            go [] = return Nothing
            go (x:xs) = do
                         -- r <- resolve x
                         r <- wait x
                         case r of 
                            Just x  -> return r
                            Nothing -> go xs
    
--------------------------
ret0f :: IO Resp
ret0f = do
    solver0f testSt1
    -- semaf <- newSemaf 1
    -- solver0f semaf testSt1
    
--------------------------







----------------------------------------------------
taskIO :: Int -> IO ()
taskIO n = do
    semaf <- newSemaf n
    
    t0    <- getCurrentTime
    --r     <- solver0f semaf testSt1
    t1    <- getCurrentTime
    let tempo = diffUTCTime t1 t0
    -- print r
    print tempo
    putStrLn $ replicate 52 '-'
    putStrLn "resposta esperada para initSt1: "
    putStrLn "[Dir,Dir,Cima,Esq,Esq,Baixo,Dir,Baixo,Dir]"
    putStrLn $ replicate 52 '-'
----------------------------------------------------

    
    





    
    


--------------------------------------------------
-- utils -----------------------------------------
pat2st :: [Moves] -> State 
pat2st ms = (foldr f id ms) initSt
    where f x rec = move x . rec

filterIO :: (B -> IO Bool) -> [B] -> IO [B]
filterIO p = go where
    go []     = return []
    go (f:fs) = do
                 mp <- fmap (func f) (p f)
                 xs <- go fs
                 return (mp xs)

          where
            func :: B -> Bool -> ([B] -> [B])
            func x isdir | isdir     = (x:) 
                         | otherwise = id


testSt0 = S 0 1 testBoard0
testBoard0 = [[1,0,3],
              [4,2,6],
              [7,5,8]]

testSt1 = S 1 0 testBoard1
testBoard1 = [[2,3,6],
              [0,1,4],
              [7,5,8]]
