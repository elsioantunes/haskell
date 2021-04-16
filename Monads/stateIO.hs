{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE MagicHash             #-}


-- import GHC.Base
-- import Control.Monad.IO.Class

import GHC.Base (IO (..), runRW#)
import GHC.Prim (RealWorld, State#, unsafeCoerce#)


{----------------------------------------------------

type State = ((Int, Int), [Int])

nextfibo :: State -> (State, Int)
nextfibo (e, lista) = ((f e, v:lista), v) where 
    v = snd (f e)
    f (a, b) = (b, a+b)

nextfiboS :: ST State Int
nextfiboS = ST nextfibo


io e = (e, ()) 
ioS = do
    liftIO (print 666)
    ST io

fibos :: Int -> ST State Int
fibos n | n < 2 = nextfiboS
        | otherwise = do
            -- print 666
            ioS 
            nextfiboS
            fibos (n-1)
               
-------------------------
listfibos n = runST (fibos n) ((0, 1), [1, 0])
main = print (listfibos 15)
-------------------------}


    
    

{----------------------------------------------------
newtype ST2 e a = ST2 {runST2 :: (e ->  IO (e, a))}

dobraIO :: Maybe Int -> IO (Maybe Int, Int)
dobraIO (Just x) = do
    return (Just (2*x), 2*x)
    
dobraSIO :: ST2 (Maybe Int) Int
dobraSIO = ST2 dobraIO    

($>) :: Num b => ST2 e a  -> (a -> ST2 e b) -> ST2 e b
ST2 g $> h = ST2 (\e -> do 
                    (e', x) <- g e
                    ST2 f   <- h x
                    (f e'))
----------------------------------------------------}
    
    


{----------------------------------------------------
type State = ((Int, Int), [Int])

func1 e = (e, v)

nextfiboS :: ST State Int
nextfiboS = ST nextfibo
----------------------------------------------------}


unsafeToST :: IO a -> ST s a
unsafeToST (IO f) = ST (unsafeCoerce# f)

runIO (IO f) s = f s


runST :: ST RealWorld a -> a
runST (ST f) = 
    case runRW# f of
        (# s, x #) -> x
        
main :: IO ()
main = print $! runST $ do
    unsafeToST $ print 666


--------------------------------------------
--------------------------------------------
newtype ST e a = ST (State# e ->  (# State# e, a #))
--------------------------------------------
instance Monad (ST e) where
   ST g >>= h = ST (\e ->
                    let
                       (# e', x #) = g e
                       ST f = h x
                    in  
                       f e')
--------------------------------------------
instance Applicative (ST e)  where
    pure a = ST (\e -> (# e, a #))
    stf <*> stx = do
                    f <- stf
                    fmap f stx
--------------------------------------------
instance Functor (ST e) where
    fmap f st = do
                  x <- st
                  return (f x)
--------------------------------------------
--------------------------------------------