data Teste a = Falha | Ret a deriving Show

maybeDivPar0 :: Int -> Int -> Teste Int
maybeDivPar0 a b | odd a     = Falha
                 | b == 0    = Falha
                 | otherwise = Ret (div a b)

 

maybeNotZero :: Int -> Teste Int
maybeNotZero b | b == 0 = Falha
               | otherwise = Ret b

maybeEven :: Int -> Teste Int
maybeEven a | odd a = Falha
            | otherwise = Ret a

maybeDivPar :: Int -> Int -> Teste Int
maybeDivPar a b = Ret (div a b)
                


vincula :: Teste a -> (a -> Teste b) -> Teste b
vincula ma f = case ma of
    Falha -> Falha
    Ret x -> f x

safeDiv0 :: Int -> Int -> Teste Int
safeDiv0 a b = 
    maybeEven a `vincula` \num ->
    maybeNotZero b `vincula` \denom ->
    maybeDivPar num denom

safeDiv :: Int -> Int -> Teste Int
safeDiv a b = do
    num <- maybeEven a
    denom <- maybeNotZero b
    return $ div num denom

    

instance Monad Teste where
    Falha >>= _ = Falha
    Ret x >>= f = f x
    
instance Applicative Teste where
    pure = Ret
    Ret f <*> Ret x = Ret (f x)

instance Functor Teste where
    fmap f (Ret x) = Ret (f x)
        