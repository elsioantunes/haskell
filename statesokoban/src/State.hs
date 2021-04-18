{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE MagicHash             #-}
module State where
import GHC.Base (IO (..), runRW#, MutVar#)
import GHC.Prim (newMutVar#, MutVar#, RealWorld, State#, unsafeCoerce#)
import Data.IORef 

----------------------------------------------------
mkgen :: IO (IO String)
mkgen = do
    r <- newIORef 0
    return $ do
        n <- readIORef r
        writeIORef r (n+1)
        return ("x" ++ show n)
        
mynames :: IO ([String], [String])
mynames = do
    gen <- mkgen
    x1 <- gen
    y1 <- gen
    z1 <- gen
    
    gen2 <- mkgen
    x2 <- gen2
    y2 <- gen2
    
    return ([x1,y1,z1],[x2,y2])

{----------------------------------------------------
data STRef s a = STRef (MutVar# s a)
newSTRef init = ST $ \s1# -> (# s2#, STRef var# #)
readSTRef (STRef var#) = ST $ \s2# -> (# State# s3#, val #)
writeSTRef (STRef var#) val = ST $ \s3# -> (# s4#, () #)
----------------------------------------------------}
runIO (IO f) s = f s


----------------------------------------------------
unsafeToST :: IO a -> ST s a
unsafeToST (IO f) = ST (unsafeCoerce# f)

bla a = ST (\e -> (# e, 2*a #))

runST :: ST RealWorld a -> a
runST (ST f) = 
    case runRW# f of
        (# s, x #) -> x

----------------------------------------------------
main :: IO ()
main = print $! runST $ do
    
    unsafeToST $ do 
        r <- mynames
        print r
        
    a <- bla 3
    b <- bla a
    c <- bla b
    return (a,b,c)
----------------------------------------------------

--------------------------------------------
--------------------------------------------
newtype ST e a = ST (State# e ->  (# State# e, a #))
--------------------------------------------
instance Monad (ST e) where
-- (>>=) :: ST b -> (b -> ST c) -> ST c
   ST g >>= h = ST (\e ->
                    let
                       (# e', x #) = g e
                       ST f = h x
                    in  
                       f e')
--------------------------------------------
instance Applicative (ST e)  where 
    pure a = ST (\e -> (# e, a #))
    stf <*> stx = do -- ap
                    f <- stf
                    fmap f stx
--------------------------------------------
instance Functor (ST e) where
    fmap f st = do  -- liftM
                  x <- st
                  return (f x)
--------------------------------------------
--------------------------------------------



-- estudar 
-- https://wiki.haskell.org/Simple_StateT_use
-- https://web.cecs.pdx.edu/~mpj/pubs/springschool95.pdf
-- https://www.fpcomplete.com/haskell/tutorial/primitive-haskell/






