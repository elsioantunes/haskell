{-# LANGUAGE CPP, MagicHash, UnboxedTuples, BangPatterns #-}

module Async where

import GHC.Conc (ThreadId(..), 
                 STM(..),
                 atomically,
                 throwSTM,
                 throwTo,
                 forkIO
                )

import Control.Exception (
        asyncExceptionFromException, asyncExceptionToException,
        SomeException(..), Exception(..),
        try, finally, catch, uninterruptibleMask_, throwIO,  mask,
        BlockedIndefinitelyOnSTM(..)
    )                

import Control.Concurrent.STM (newEmptyTMVarIO, putTMVar, readTMVar, TMVar(..))   
{----------------------------------------------------
Video sobre memoria transacional (do Emilio)
https://youtu.be/3UOqmZpPeK8?list=PLR2tpXhN7CHckZTqYxEfmZ4Z5Tha2Y41W
----------------------------------------------------}

import Control.Concurrent (putMVar, takeMVar, newEmptyMVar, newMVar, modifyMVar, forkOS, MVar(..))

import GHC.Exts (fork#, RealWorld(..), State#(..))
-- import Control.Concurrent.Async (wait, withAsync)
-- https://www.fpcomplete.com/haskell/library/async/


import Data.IORef (IORef (..), atomicModifyIORef, newIORef)

-- import Control.Monad.Par.IO
-- import Control.Monad.Par.Class
-- import Control.Monad.IO.Class

--------------------------------------------------------------------------
--------------------------------------------------------------------------
data Async a = Async
  { asyncThreadId :: {-# UNPACK #-} !ThreadId
                  -- ^ Returns the 'ThreadId' of the thread running
                  -- the given 'Async'.
  , _asyncWait    :: STM (Either SomeException a)
  }




{----------------------------------------------------
    forkFinally 
        Fork a thread 
        and call the supplied function 
        when the thread is about to terminate, 
        with an exception or a returned value. 
        The function is called with asynchronous exceptions masked.    

    forkFinally action and_then =
      mask $ \restore ->
        forkIO $ try (restore action) >>= and_then
    
    This function is useful 
    for informing the parent 
    when a child terminates, 
    for example.
----------------------------------------------------}



----------------------------------------------------
withAsync  :: IO a -> (Async a -> IO b) -> IO b     


withAsync  action inner = do 
    var <- newEmptyTMVarIO
    
    
    
    
    t <- mask      $ \restore -> do  -- asyncu
        t       <- forkIO (try (restore action) >>= atomically . putTMVar var)
        let a   = Async t (readTMVar var)   
        r       <- restore (inner a) `catchAll` \e -> do
                        uninterruptibleMask_ (cancel a)
                        throwIO e
        uninterruptibleMask_ (cancel a)
        return r
    return t


{-
-- When the function returns or throws an exception,
-- 'uninterruptibleCancel' is called on the @Async@.
--
-- > withAsync action inner = mask $ \restore -> do
-- >   a <- async (restore action)
-- >   restore (inner a) `finally` uninterruptibleCancel a
--
-- This is a useful variant of 'async' that ensures an @Async@ is
-- never left running unintentionally.
--
-- Note: a reference to the child thread is kept alive until the call
-- to `withAsync` returns, so nesting many `withAsync` calls requires
-- linear memory.

-- | Spawn an asynchronous action in a separate thread, 
--   and pass its @Async@ handle to the supplied function.
-}    

catchAll :: IO a -> (SomeException -> IO a) -> IO a
catchAll = catch




















-- | Spawn an asynchronous action in a separate thread.

async      :: IO a -> IO (Async a) 


async      action       = do 
    var <- newEmptyTMVarIO

    -- t <- forkFinally action (\r -> 
    --                             atomically $ putTMVar var r) -- 3a
    -- slightly faster:
    t <- mask      $ \restore -> do  -- asyncu
        t       <- forkIO (try (restore action) >>= atomically . putTMVar var)
                   
           
           
    
    
        return t
    return (Async t (readTMVar var))














----------------------------------------------------
----------------------------------------------------
-- | Wait for an asynchronous action to complete, and return its
-- value.  If the asynchronous action threw an exception, then the
-- exception is re-thrown by 'wait'.
--
-- > wait = atomically . waitSTM
--
{-# INLINE wait #-}
wait :: Async a -> IO a
wait = tryAgain . atomically . waitSTM
  where
    -- See: https://github.com/simonmar/async/issues/14
    tryAgain f = f `catch` \BlockedIndefinitelyOnSTM -> f


-- | A version of 'wait' that can be used inside an STM transaction.
--
waitSTM :: Async a -> STM a
waitSTM a = do
   r <- waitCatchSTM a
   either throwSTM return r



-- | Cancel an asynchronous action by throwing the @AsyncCancelled@
-- exception to it, and waiting for the `Async` thread to quit.
-- Has no effect if the 'Async' has already completed.
--
-- > cancel a = throwTo (asyncThreadId a) AsyncCancelled <* waitCatch a
--
-- Note that 'cancel' will not terminate until the thread the 'Async'
-- refers to has terminated. This means that 'cancel' will block for
-- as long said thread blocks when receiving an asynchronous exception.
--
-- For example, it could block if:
--
-- * It's executing a foreign call, and thus cannot receive the asynchronous
-- exception;
-- * It's executing some cleanup handler after having received the exception,
-- and the handler is blocking.
{-# INLINE cancel #-}
cancel :: Async a -> IO ()
cancel a@(Async t _) = throwTo t AsyncCancelled <* waitCatch a


-- | A version of 'waitCatch' that can be used inside an STM transaction.
--
{-# INLINE waitCatchSTM #-}
waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async _ w) = w



-- | Wait for an asynchronous action to complete, and return either
-- @Left e@ if the action raised an exception @e@, or @Right a@ if it
-- returned a value @a@.
--
-- > waitCatch = atomically . waitCatchSTM
--
{-# INLINE waitCatch #-}
waitCatch :: Async a -> IO (Either SomeException a)
waitCatch = tryAgain . atomically . waitCatchSTM
  where
    -- See: https://github.com/simonmar/async/issues/14
    tryAgain f = f `catch` \BlockedIndefinitelyOnSTM -> f







-- | The exception thrown by `cancel` to terminate a thread.
data AsyncCancelled = AsyncCancelled
  deriving (Show, Eq)

instance Exception AsyncCancelled where
#if __GLASGOW_HASKELL__ >= 708
  fromException = asyncExceptionFromException
  toException = asyncExceptionToException
#endif


