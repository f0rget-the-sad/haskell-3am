import Control.Monad (ap, liftM)

data Log a = Log [String] a deriving Show

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg x = Log [msg] (f x)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = let
    Log fmsg fval = f x
    Log gmsg gval = g fval
    in Log (fmsg ++ gmsg) gval

returnLog :: a -> Log a
returnLog a = Log [] a

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log msga a) f = let
    Log msgb b = f (a)
    in Log (msga ++ msgb) b

instance Functor Log where
    fmap = liftM

instance Applicative Log where
    pure = return
    (<*>) = ap

instance Monad Log where
    return = returnLog
    (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList ini funcs = foldr (\f l -> bindLog l f) (return ini) (reverse funcs)

-- Tests --
add1Log = toLogger (+ 1) "added one"
mult2Log = toLogger (* 2) "multiplied by 2"
logFuncs = [add1Log, mult2Log, \x -> Log ["multiplied by 100"] (x * 100)]

run x = if any (==False) x then error "Tests failed!" else "Test Passed!"

logTests = [
    show (add1Log 3) == show (Log ["added one"] 4),
    show (mult2Log 3) == show (Log ["multiplied by 2"] 6),
    show (execLoggers 3 add1Log mult2Log) ==
        show (Log ["added one","multiplied by 2"] 8),
    show (Log ["nothing done yet"] 0 `bindLog` add1Log) ==
        show (Log ["nothing done yet","added one"] 1),
    show (Log ["nothing done yet"] 3 `bindLog` add1Log `bindLog` mult2Log)  ==
    show (Log ["nothing done yet","added one","multiplied by 2"] 8),
    show (execLoggersList 3 logFuncs) == show (
        Log ["added one","multiplied by 2","multiplied by 100"] 800)

 ]
