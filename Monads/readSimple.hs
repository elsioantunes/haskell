{-# LANGUAGE GeneralizedNewtypeDeriving #-}
------------------------------------
------------------------------------
msg :: Int -> String
msg n = case (mod n 3) of
            0 -> show n ++ " eh multiplo de 3"
            1 -> show n ++ " deixa resto 1 na div por 3"
            2 -> show n ++ " deixa resto 2 na div por 3"
-----------------------------------------
func1r :: Reader Int Bool
func1r = R even

func2r :: Reader Int Int
func2r = R (*2)

func3r :: Reader Int String
func3r = R msg
------------------------------------------
sistemaR :: Reader Int (Bool, Int, String)
sistemaR = do
               b <- func1r
               d <- func2r
               s <- func3r
               return (b, d, s)
------------------------------------------
main = do
        print $ runReader sistemaR 10
------------------------------------
------------------------------------




------------------------------------
------------------------------------
newtype Rder e a = Rr (e->a)
------------------------------------
mdobra :: Rder Int Int
mdobra = Rr (*2)

ehPar :: Rder Int Bool
ehPar = Rr even

cochetes :: Rder Int String
cochetes = Rr (\n -> "{" ++ show n ++ "}")
------------------------------------
runRder :: Rder e a -> (e -> a)
runRder (Rr e) = e

retRder :: a -> Rder b a
retRder a = Rr (const a)

(>-) :: Rder e a -> (a -> Rder e b) -> Rder e b
(Rr g)  >-  f  = Rr (\e -> b e)
      where
        b x = runRder ((f . g) x) x

sis :: Rder Int (Int, Bool, String)
sis = mdobra    >- \a ->
      ehPar     >- \b ->
      cochetes  >- \c ->
      retRder (a,b,c)
------------------------------------
------------------------------------






------------------------------------
------------------------------------
db = (*2)
ep = even
ch = \n -> "{" ++ show n ++ "}"

-- type R0 e a = [e->a]
------------------------------------
-- ($>) :: R0 e a -> (a -> R0 e b) -> R0 e b
[g] $> f  = [(\e -> b e)] 
      where
        b x = runR0 ((f . g) x) x

-- runR0 :: [a] -> a
runR0 [e] = e

-- retR0 :: a -> R0 e a
retR0 a = [const a]

-- rdb :: R0 Int Int
rdb = [db]

-- rep :: R0 Int Bool
rep = [ep]

-- rch :: R0 Int String
rch = [ch]
        
sis0' :: [Integer -> (Integer, Bool, [Char])]
sis0' = rdb $> \a -> 
        rep $> \b -> 
        rch $> \c -> 
        retR0  (a, b, c)
        
teste = runR0 sis0' 666
------------------------------------
sis0 = (\e -> b e)
    where
        -- b x = (db x, ep x, ch x)
        -- b x = (\a b c -> (a, b, c))  (db x) (ep x) (ch x)
        b x = (\a -> (\b -> (\c -> (a, b, c))(ch x))(ep x)) (db x) 
        
------------------------------------
------------------------------------
        
        
        




------------------------------------
------------------------------------
newtype Reader e a = R {runReader :: e->a}
    deriving (Functor, Applicative, Monad)
    -- GeneralizedNewtypeDeriving
------------------------------------
{-
instance Monad (Reader e) where
    R g >>= f = R (\e -> b e)
      where
        b x = runReader ((f . g) x) x
    ------------------------------------
    comp >>= f = R $ \e -> let
        b e = runReader (h e) e
        h e = f (runReader comp e)
        in b e
    ------------------------------------
    comp >>= f = R (\e -> b e)
      where
        b e = runReader (h e) e
        h e = f (runReader comp e)
-}
--------------------------------------------------- 
{-
instance Functor (Reader e) where
    fmap f rx = do                    -- GeneralizedNewtypeDeriving
                  x <- rx
                  return (f x)

    ------------------------------------
    fmap :: (a -> b) -> Reader e a -> Reader e b
    ------------------------------------
    fmap f (R g) = R (f . g)          -- sabendo que g é uma função... 
    ------------------------------------
    fmap f rx = R $ \e -> let
        x = runReader rx e
        in f x
    ------------------------------------
    fmap f (R func) = R $ \e -> let
        x = func e
        in f x
    ------------------------------------
-}
---------------------------------------------------
{-
instance Applicative (Reader e) where
    pure a = R (const a)

    rf <*> rx = do        -- GeneralizedNewtypeDeriving
        f <- rf
        fmap f rx
    ------------------------------------
    (<*>) :: Reader e (a -> b) -> Reader e a -> Reader e b
    ------------------------------------
    rf <*> rx = R $ \e -> let
        f = runReader rf e
        x = runReader rx e
        in f x
    ------------------------------------
    R func <*> R val = R $ \e -> let
        f = func e
        x = val e
        in f x
-}



                  
{------------------------------------------
instance Show (Reader e) where
    show (R e a) = show a
------------------------------------------}
