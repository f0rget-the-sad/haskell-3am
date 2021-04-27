data List a = Nil | Cons a (List a)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x xs) = x : fromList xs

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x (toList xs)

data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf _) = 0
height (Node l r) = (max (height l) (height r)) + 1

size :: Tree a -> Int
size (Leaf _) = 1
size (Node l r) = (size l) + (size r) + 1

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t (0,0)
    in s `div` c
  where
    go (Leaf x) (c, s) = (c + 1, s + x)
    go (Node l r) p = let
        (lc, ls) = (go l p)
        (rc, rs) = (go r p) in
            (lc + rc, ls + rs)
