module State where

import Reader
import Writer

import Control.Monad

newtype State s a = State {runState :: s -> (a,s) }

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Functor (State s) where
  fmap = liftM

instance Monad (State s) where
  return a = State $ \st -> (a,st)

  m >>= k = State $ \st ->
    let (a, st') = runState m st
        m' = k a
    in runState m' st'

execState :: State s a -> s -> s
execState m s = snd (runState m s)

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

get :: State s s
get = State $ \st -> (st, st)

put :: s -> State s ()
put st = State $ \_ -> ((), st)

tick :: State Int Int
tick = do
    n <- get
    put (n+1)
    return n

modify :: (s -> s) -> State s ()
modify f = do
    s <- get
    put (f s)


{-
newtype State s a = State {runState :: s -> (a,s) }
data Reader r a = Reader { runReader :: (r -> a) }

local' :: (r -> r') -> Reader r' a -> Reader r a
local' f m = Reader $ \e -> runReader m (f e)
return a = State $ \st -> (a,st)
 -}
readerToState :: Reader r a -> State r a
readerToState m = State $ \e -> ((runReader m e), e)

writerToState :: Monoid w => Writer w a -> State w a
writerToState m = let (val, w) = runWriter m in
    State $ \e -> (val, mappend e w)
