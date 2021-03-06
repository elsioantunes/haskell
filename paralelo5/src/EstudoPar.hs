{-# OPTIONS_GHC -fno-warn-unused-top-binds  -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}
{-# LANGUAGE 
    RankNTypes, 
    NamedFieldPuns, 
    BangPatterns, 
    ExistentialQuantification,  
    GeneralizedNewtypeDeriving,
    DeriveFunctor,
    CPP #-}
------------------------------------------------------------------------------        
{-  Notas
        In Dynamic Programming ,
            No overhead for recursion, less overhead for maintaining the table.
            The regular pattern of the table accesses may be used to reduce time or space requirements.
        
        In Memorization,
            Some subproblems do not need to be solved.
        ---------------------------------------------------
            
        In Memoization we go with 'Top down approach'
            where we save each function call in a cache 
                (We break down the bigger problem into smaller sub problems)
            and call back from there. 
            Its a bit (expensive :: caro) (as it :: ja que) involves recursive calls.
        
        In Dynamic Programming we go with 'Bottom up approach'
                (We start from smallest sub problem and reach the bigger problem.)
            where we maintain a table, bottom up 
            by solving subproblems 
            using the data saved in the table
            
                Top-down
                    First you say 
                    I will take over the world. 
                    How will you do that? 
                    You say I will take over Asia first. 
                    How will you do that? 
                    I will take over India first. 
                    I will become the Chief Minister of Delhi, 
                        etc. etc.
                
                Bottom-up
                    You say 
                    I will become the CM of Delhi. 
                    Then will take over India, 
                    then all other countries in Asia 
                    and finally I will take over the world.
        
        Note:
            Both are applicable to problems with Overlapping sub-problems.
            
            Memoization performs comparatively poor to DP 
            due to the overheads involved during recursive function calls.
            
            The asymptotic time-complexity remains the same.
        ---------------------------------------------------
        problema da escada 
        https://leetcode.com/problems/climbing-stairs/
        ---------------------------------------------------
        Optimization technique 
            to speed up computer programs 
            (by having :: fazendo com que) function calls 
            (avoid repeating :: evitem repetir) the calculation of results 
            for previously-processed inputs.
        ---------------------------------------------------
        Memoization and DP, conceptually, is really the same thing. Because: 
            consider the definition of DP: 
                "overlapping subproblems" "and optimal substructure"
            Memoization fully possesses these 2.
        
        Memoization is DP 
            with the risk of stack overflow is the recursion is deep. 
            DP bottom up does not have this risk.
        
        Memoization needs a hash table. 
            So additional space, and some lookup time.
        
        memoization is a subset of DP, 
            in a sense that a problem solvable by memoization 
            will be solvable by DP, 
            but a problem solvable by DP 
            might not be solvable by memoization 
            (because it might stack overflow).
            
        they have minor differences in performance.
        
        ---------------------------------------------------
        DP has the potential to transform exponential-time brute-force solutions into polynomial-time algorithms.
        
        ---------------------------------------------------
        Memoization is 
        an easy method to track previously solved solutions 
            often implemented as a hash key value pair, 
            as opposed to 
            tabulation which is often based on arrays
        
        (so that :: de modo que)
        they aren't recalculated 
        when they are encountered again. 
        
        It can be used in both bottom up or top down methods.
        
        Dynamic programming 
        is a method to solve certain classes of problems 
        by solving recurrence relations/recursion 
        and storing previously found solutions 
        via either tabulation or memoization. 
        
        Memoization 
        is a method to keep track of solutions 
        to previously solved problems 
        and can be used with any function 
        that has unique deterministic solutions 
        for a given set of inputs.
        ---------------------------------------------------
        
        If all subproblems must be solved at least once, 
        
        a bottom-up DP algorithm 
        usually (outperforms :: supera, performa melhor que) 
        
        a top-down memoized algorithm 
        by a constant factor
            
            # No overhead for recursion and 
            less overhead for maintaining table
            (overhead :: sobrecarga)
            
            # There are 
            some problems (for which :: para os quais)
            the regular pattern of table accesses 
            in the DP algorithm 
            can be exploited 
            to reduce the time 
            or space requirements 
            (even further :: ainda mais)
            
        If some subproblems in the subproblem space 
        need NOT be solved (at all :: de forma alguma), 
        the memoized solution has the advantage 
        of solving only those subproblems 
        that are definitely required            

        https://stackoverflow.com/questions/6184869/what-is-the-difference-between-memoization-and-dynamic-programming
        
------------------------------------------------------------------------------                
        
        Avalia��o pregui�osa vs avalia��o avida/ansiosa/rigorosa (lazy vs eager)
          
          Avalia��o pregui�osa: 
           atrasar a computa��o 
           at� um ponto em que o resultado 
           � considerado suficiente (David A. Watt - Design Concepts)
          
          # Eager/strict/greedy/Ansiosa/Gulosa - vantagem
             . Economia de mem�ria e aumento de velocidade.
             . Facil implementa��o, acompanhamento da ordem da execu��o e consequente debug
             . 
             
             

          # Lazy/Pregu - vantagem
             Desempenho por evitar computa��o desnecess�ria
             previs�o de erros em tempo de compila��o
             constru��o de estruturas de dados infinitas
             constru��o de FUN��ES regulares
             
             
             
             
          # Lazy/Pregu - desvantagem
             Necessita 
             estruturas de dados intermedi�rios 
             para representar express�es n�o avaliadas

            
            
            
       
-}

module EstudoPar where

import Control.DeepSeq (NFData, rnf, deepseq)

import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef) 
-- https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-IORef.html

import Control.Concurrent (
            MVar, newEmptyMVar, putMVar, takeMVar, readMVar, forkIO, 
            forkOn, myThreadId, threadCapability) -- hiding (yield)

-- https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Concurrent.html
{-  
    Concurrency is "lightweight", which means that both 
    THREAD CREATION and (CONTEXT SWITCHING OVERHEADS :: sobrecarga de mudan�a de contexto)
    are extremely low
    
    SCHEDULING
        SCHEDULING of Haskell threads 
        is done internally in the Haskell runtime system, 
        and doesn't make use of any operating system-supplied thread packages.
    
    
    MVars
        Haskell threads can communicate via MVars, 
        (a kind of synchronised mutable variable)
        Several common concurrency abstractions can be built from MVars, 
        and these are provided by the Control.Concurrent library. 
    
    In GHC, threads may also communicate via exceptions.
    
    ThreadId
        A ThreadId is an abstract type 
        representing a handle to a thread. 
        ThreadId is an instance of Eq, Ord and Show, 
        where the Ord instance 
        implements an arbitrary total ordering over ThreadIds. 

        The Show instance 
        lets you convert an arbitrary-valued ThreadId to string form;

        showing a ThreadId value is occasionally useful 
        when debugging or diagnosing the behaviour of a concurrent program.
    
        Note: in GHC, 
        if you have a ThreadId, 
        you essentially have a pointer to the thread itself. 
        This means the thread itself can't be garbage collected 
        until you drop the ThreadId. 

        This misfeature will hopefully be corrected at a later date.
        

    forkIO 
        Creates a new thread
            to run the IO computation passed as the first argument, 
            and returns the ThreadId of the newly created thread.

            The new thread will be a lightweight, unbound thread. 

        mask
            The new thread inherits the masked state of the parent

        Exceptions
            The newly created thread has an exception handler 
            that discards the exceptions BlockedIndefinitelyOnMVar, 
            BlockedIndefinitelyOnSTM, and ThreadKilled, 
            and passes all other exceptions 
            to the uncaught exception handler
    
    
    forkOn :: Int -> IO () -> IO ThreadIdSource
        Like forkIO, 
        but lets you specify on which capability the thread should run. 
        Unlike a forkIO thread, 
        a thread created by forkOn 
        will stay on the same capability for its entire lifetime 
        (forkIO threads can migrate between capabilities 
        according to the scheduling policy). 

        forkOn is useful for overriding the scheduling policy 
        when you know in advance 
        how best to distribute the threads.

        The Int argument specifies a capability number 
        (see getNumCapabilities). 
        Typically capabilities correspond to physical processors, 
        but the exact behaviour is implementation-dependent. 

        The value passed to forkOn is interpreted modulo 
        the total number of capabilities as returned by getNumCapabilities.

        GHC note: 
        the number of capabilities is specified by the +RTS -N option 
        when the program is started. 
        Capabilities can be fixed to actual processor cores 
        with +RTS -qa if the underlying operating system supports that, 
        although in practice this is usually unnecessary 
        (and may actually degrade performance in some cases 
        - experimentation is recommended).
    
    forkFinally 
        Fork a thread 
        and call the supplied function 
        when the thread is about to terminate, 
        with an exception or a returned value. 
        The function is called with asynchronous exceptions masked.    
    
    throwTo :: Exception e => ThreadId -> e -> IO ()
    
    throwTo (raises :: levanta) an arbitrary exception 
    in the target thread 
    
    Exception delivery 
    synchronizes between the source and target thread :  
    (thread fonte e thread destino)  
    throwTo does not return until the exception has been raised 
    in the target thread. 
    
    The calling thread can thus be certain 
    that the target thread has received the exception. 
    
    Exception delivery is also atomic with respect to other exceptions. 
    Atomicity is a useful property to have 
    when dealing with race conditions: 
    e.g. if there are two threads 
    that can kill each other, 
    it is guaranteed that only one of the threads 
    will get to kill the other.
    
    
    Whatever work the target thread was doing 
    when the exception was raised is not lost: 
    the computation is suspended until required by another thread.
    
    If the target thread is currently making a foreign call, 
    then the exception will not be raised 
    (and hence throwTo will not return) 
    until the call has completed. 
    
    This is the case regardless of whether 
    the call is inside a mask or not. 
    
    However, in GHC a foreign call can be annotated as interruptible, 
    in which case a throwTo will cause the RTS to attempt 
    to cause the call to return; 
    see the GHC documentation for more details.
    
    Important note: 
    the (behaviour :: comportamento) of throwTo 
    differs from that described in the paper 
    "Asynchronous exceptions in Haskell" 
    (http://research.microsoft.com/~simonpj/Papers/asynch-exns.htm). 
    
    In the paper, throwTo is non-blocking; 
    but the library implementation adopts a more synchronous design 
    in which throwTo does not return until the exception is received 
    by the target thread. 
    The trade-off is discussed in Section 9 of the paper. 
    
    Like any blocking operation, 
    throwTo is therefore interruptible (see Section 5.3 of the paper). 
    Unlike other interruptible operations, however, 
    throwTo is always interruptible, 
    even if it does not actually block.
    
    There is no guarantee that the exception will be delivered promptly, 
    (although :: embora) the runtime will (endeavour :: esfor�ar-se) 
    to (ensure :: ensure) that arbitrary delays don't occur. 
    
    In GHC, an exception can only be raised 
    when a thread (reaches :: alcan�a) a safe point, 
    where a safe point is where memory allocation occurs. 
    
    Some loops do not perform any memory allocation 
    inside the loop and (therefore :: portanto) 
    cannot be interrupted by a throwTo.
    
    If the target of throwTo is the calling thread, 
    then the behaviour is the same as throwIO, 
    except that the exception is thrown as an asynchronous exception. 
    
    This means that 
    if there is an enclosing pure computation, 
    which would be the case if the current IO operation 
    is inside unsafePerformIO or unsafeInterleaveIO, 
    that computation is not permanently replaced by the exception, 
    but is suspended as if it had received an asynchronous exception.
    
    Note that 
    if throwTo is called with the current thread as the target, 
    the exception will be thrown 
    even if the thread is currently inside mask or uninterruptibleMask.
    
-}

import GHC.Conc (numCapabilities, ThreadId)

import Control.Monad (replicateM, when, forM_, ap)
import Control.Monad.Fix (MonadFix (mfix))

import System.IO.Unsafe (unsafePerformIO)
import GHC.IO.Unsafe (unsafeDupableInterleaveIO)
import Control.Exception (Exception, throwIO, catch, 
    SomeException, try, mask, 
    BlockedIndefinitelyOnMVar (..))

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-
    Starting point: A Poor Man�s Concurrency Monad (Claessen JFP�99)
    (simulate concurrency in a sequential)
    
    - Par computations produce a lazy Trace
    - A scheduler consumes the Traces, and switches between multiple threads
    (y lightweight nonpreemptive threads, with a parallel scheduler)

        "Par is a CPS monad" -- Simon
        Continuation Passing Style (CPS): 
        https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style
        
        "A Monad for Deterministic Parallelism" - slides
        http://www.cs.ox.ac.uk/ralf.hinze/WG2.8/28/slides/simon.pdf
        
        artigo (12 paginas)
        https://www.microsoft.com/en-us/research/wp-content/uploads/2011/01/monad-par.pdf
        
        Streams
        https://github.com/chessai/rstream/blob/master/src/RStream.hs


-}


{----------------------------------------------------
sinal :: Par Int
sinal = do
         x <- new
         fork (put x 3)
         r <- get x
         return (r + 1)

-- sinal2 =  New  $ \x -> Fork (Put x 3 $ Done)  (Get x $ \r -> c (r + 1))   
----------------------------------------------------}


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
instance Eq (IVar a) where
  (IVar r1) == (IVar r2) = r1 == r2

instance NFData (IVar a) where
  rnf !_ = ()

data Trace = forall a . New (IVc a) (IVar a -> Trace)
           | forall a . Put (IVar a) a Trace
           | forall a . Get (IVar a) (a -> Trace)
           |            Fork Trace Trace
           |            Done
           |            Msg String Trace

data Sched = Sched
    { no       :: {-# UNPACK #-} !Int,
      workpool :: IORef [Trace],
      idle     :: IORef [MVar Bool],
      scheds   :: [Sched]
    }    

-- Par is a CPS monad
newtype Par a = Par {runCont :: (a -> Trace) -> Trace} 
    deriving (Functor)

newtype IVar a = IVar (IORef (IVc a))

data IVc a = Empty | Blocked [a -> Trace] | Full a   -- IVarContents
    
instance Monad Par where
    Par g >>= f  = Par (\c -> func1 c)
      where
        func1 c = g (\x -> func2 x c)
        func2 x = runCont (f x)

{-
    Par g >>= f = Par (\c -> g (h c))
        where 
            h c a = runCont (f a) c
-}
{-
    m >>= k  = Par (\c -> runCont m (\a -> runCont (k a) c))
-}

instance Applicative Par where
   parf <*> parx = do     -- (<*>) = ap
                    f <- parf
                    fmap f parx
   pure a = Par ($ a)
{-
    Can't make a derived instance of `Applicative Par'
    even with cunning GeneralizedNewtypeDeriving
        cannot eta-reduce the representation type enough
---------------------------------------
instance Functor Par where
    fmap f par = do
                  x <- par
                  return (f x)
    -- fmap f m = Par (\c -> (runCont m) (c . f))
    ----------------------------    
    even with cunning  "GeneralizedNewtypeDeriving"
    You need           "DeriveFunctor" 
        to derive an instance for (this class [?])
-}
-------------------------------------------------------------------------------
par :: ((a -> Trace) -> Trace) -> Par a
par f = Par f

put0 :: IVar a -> a -> (() -> Trace) -> Trace
put0 v a c = Put v a (c ())

forkruncont0 :: (() -> Trace) -> Par () -> Trace
forkruncont0 c p = Fork (runcont p done0) (c ())

get0 :: IVar a -> (a -> Trace) -> Trace
get0 v c = Get v c

runcont :: Par a -> (a -> Trace) -> Trace
runcont p done0 = runCont p done0

done0 :: () -> Trace
done0 () = Done

-------------------------------------------------------------------------------
msg :: String -> Par ()
msg s = Par $ \c -> Msg s (c ())

put :: NFData a => IVar a -> a -> Par ()
put v a = Par (\c -> deepseq a (put0 v a c))
-- put v a = deepseq a (Par (\c -> (put0 v a c))) -- Par 0.1

put_ :: IVar a -> a -> Par ()
put_ v !a = Par $ \c -> Put v a (c ())

fork :: Par () -> Par ()
fork p = Par (\c -> forkruncont0 c p)
-- fork p = Par (\c -> Fork (runCont p (\_ -> Done)) (c ()))



get :: IVar a -> Par a
get v = par (\c -> get0 v c)



new :: Par (IVar a)
new = Par (New Empty)
-- new = Par (\c -> New c)   -- slides

spawn :: NFData a => Par a -> Par (IVar a)
spawn p  = do r <- new;  fork (p >>= put r);   return r

spawnP :: NFData a => a -> Par (IVar a)
spawnP = spawn . return 



runPar :: Par a -> a
runPar = unsafePerformIO . runPar_internal
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------












-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- atomicModifyIORef :: IORef a -> (a -> (a, b)) -> IO b
reschedule1 :: Sched -> IO ()
reschedule1 queue = do
    e <- atomicModifyIORef (workpool queue) picTrace1
    case e of   
        Nothing -> steal queue
        Just t  -> sched queue t

------------------------------------------------
states1 :: [IORef [Trace]] -> IORef [MVar Bool] -> [(Int, Sched)]
states1 wkps idl = [
    (workN, Sched { no       = workN, 
                    workpool = wp,
                    idle     = idl,
                    scheds   = map snd $ states1 wkps idl
                   })
         | (workN, wp) <- zip [0..] wkps 
    ]

----------------------------------------------------
runparinternal :: MVar (IVc a) -> Par a -> IO ()
runparinternal  m par_a = do
    -- print par_a
    
    (main_cpu, _) <- threadCapability =<< myThreadId
    
                     -- :: [IORef [Trace]] *4
    workpools1    <- replicateM numCapabilities (newIORef [])
    
                     -- :: IORef [MVar Bool]
    idle1         <- newIORef []  
    
    forM_ (states1 workpools1 idle1) $ \(cpu, state) -> do
        
        if (cpu == main_cpu) then  
            forkOn cpu $ do

                v <- newIORef Empty

                sched state $
                    runCont (put_ (IVar v) =<< par_a) (const Done)

                putMVar m =<< readIORef v


        else 
            forkOn cpu $ do
                e <- atomicModifyIORef (workpool state) picTrace1
                case e of   
                    Nothing -> steal state
                    Just t  -> sched state t
        
------------------------------------------------
steal :: Sched -> IO ()
steal q@Sched { idle, 
                scheds, 
                no      = meu_nr 
              } 
    = do
       -- putStrLn ("meu_nr: " ++ show meu_nr)
       go scheds

  where
    go [] = do 
        m <- newEmptyMVar
        r <- atomicModifyIORef idle (pushMvar m)

        if length r == numCapabilities - 1 then 
            mapM_ (\m -> putMVar m True) r

        else do
            done <- takeMVar m
        
            if done then 
                return ()
            else 
                go scheds
             
    go (chd:chds) | no chd == meu_nr = go chds | otherwise = do
        r <- atomicModifyIORef (workpool chd) picTrace1
        case r of
            Nothing -> go chds
            Just t  -> sched q t



------------------------------------------------
-- atomicModifyIORef :: IORef a -> (a -> (a, b)) -> IO b
------------------------------------------------
picTrace1 :: [Trace] -> ([Trace], Maybe Trace)
picTrace1 []     = ([], Nothing)
picTrace1 (x:xs) = (xs, Just x)

------------------------------------------------
pushMvar :: MVar Bool -> [MVar Bool] -> ([MVar Bool], [MVar Bool])
pushMvar x xs =  (x:xs, xs)

------------------------------------------------
get1 :: (a -> Trace) -> Sched -> (Trace -> IO ()) 
        -> IVc a -> (IVc a, IO ())
get1 c queue loop e = 
    case e of
        Full a     -> (Full a,         loop (c a))
        Empty      -> (Blocked [c],    reschedule1 queue)
        Blocked cs -> (Blocked (c:cs), reschedule1 queue)

------------------------------------------------
put1 :: a -> IVc a -> (IVc a, [a -> Trace])
put1 a e = 
    case e of
        Empty      -> (Full a, [])
        Blocked cs -> (Full a, cs)
        Full _     -> error "multiple put"

------------------------------------------------
sched :: Sched -> Trace -> IO ()
sched queue t = loop t where
    
    loop t = case t of
        New a f           -> do
                              r <- newIORef a
                              loop $ f (IVar r)
            
        Get (IVar v) c    -> do
                               ret <- atomicModifyIORef v (get1 c queue loop)
                               ret
        
        Put (IVar v) a t  -> do
                              cs <- atomicModifyIORef v (put1 a)
                              mapM_ (pushWork queue . ($a)) cs
                              loop t

        Fork child parent -> do
                              pushWork queue child
                              loop parent

        Msg x t           -> do
                              putStrLn x
                              loop t

        Done              -> reschedule1 queue
        
    
    pushWork :: Sched -> Trace -> IO ()
    pushWork Sched {workpool, idle} t = do
        atomicModifyIORef workpool $ \ts -> (t:ts, ())
        idles <- readIORef idle
        when (not (null idles)) $ do
            r <- atomicModifyIORef idle (\is -> 
                case is of
                    []     -> ([], return ())
                    (i:is) -> (is, putMVar i False))
            r
            
-----------------------------------------------------------------------------
{-# INLINE runPar_internal #-}
runPar_internal :: Par a -> IO a
runPar_internal  par_a = do
    m <- newEmptyMVar
    runparinternal m par_a
    r <- takeMVar m
    
    case r of
      Full a -> return a
      _      -> error "no result"
            

{-
    stack exec estudopar --RTS -- +RTS -N -s -ls
-}