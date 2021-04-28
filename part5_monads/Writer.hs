module Writer where

import Data.Monoid
import Control.Monad

newtype Writer w a = Writer {runWriter :: (a, w)}

writer :: (a, w) -> Writer w a
writer = Writer

execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)

evalWriter :: Writer w a -> a
evalWriter m = fst $ runWriter m

instance (Monoid w) => Applicative (Writer w) where
  pure = return
  (<*>) = ap

instance (Monoid w) => Functor (Writer w) where
  fmap = liftM

instance (Monoid w) => Monad (Writer w) where
  return x = Writer (x, mempty)
  m >>= k =
    let (x, u) = runWriter m
        (y, v) = runWriter $ k x
    in Writer (y, mappend u v)

tell :: Monoid w => w -> Writer w ()
tell w = writer ((), w)

calc :: (Int -> Int -> Int) -> Int -> Int -> Writer String Int
calc op arg1 arg2 = do
    let res = arg1 `op` arg2
    if abs res < 128 then
        -- tell "ok"
        return res
    else do
        tell "overflow"
        return res

type Shopping = Writer (([String], Sum Integer)) ()

purchase :: String -> Integer -> Shopping
purchase itemName cost = tell ([itemName], (Sum cost))

total :: Shopping -> Integer
total = getSum . snd . execWriter

items :: Shopping -> [String]
items = fst . execWriter

tobuy = [
    ("Jeans", 19200),
    ("Water", 180),
    ("Lettuce", 328)
    ]

shopping1 :: Shopping
shopping1 = do
    mapM_ (\(item, cost) -> purchase item cost) tobuy

test1 = total shopping1 == foldr (\(_, c) -> (+) c) 0 tobuy
test2 = items shopping1 == foldr (\(i, _) -> (:) i) [] tobuy

tests = all (==True) [test1, test2]
