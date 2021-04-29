import Control.Monad

import State

fibStep :: State (Integer, Integer) ()
fibStep = do
    (n1, n2) <- get
    put (n2, (n1 + n2))
    return ()

execStateN :: Int -> State s a -> s -> s
execStateN n m  = execState (replicateM n m)

fib :: Int -> Integer
fib n = fst $ execStateN n fibStep (0, 1)
