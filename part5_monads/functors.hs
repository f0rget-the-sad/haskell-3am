import Data.Char

data Point3D a = Point3D a a a deriving Show

instance Functor Point3D where
    fmap f (Point3D a1 a2 a3) = Point3D (f a1) (f a2) (f a3)

data GeomPrimitive a =
    Point (Point3D a) |
    LineSegment (Point3D a) (Point3D a)

instance Functor GeomPrimitive where
    fmap f (Point p) = Point (fmap f p)
    fmap f (LineSegment p1 p2) = LineSegment (fmap f p1) (fmap f p2)

data Tree a =
    Leaf (Maybe a) |
    Branch (Tree a) (Maybe a) (Tree a)
    deriving Show

instance Functor Tree where
    fmap f (Leaf a) = Leaf (fmap f a)
    fmap f (Branch l a r) = Branch (fmap f l) (fmap f a) (fmap f r)

data Entry k1 k2 v = Entry (k1, k2) v deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v] deriving Show

instance Functor (Entry k1 k2) where
    fmap f (Entry keys v) = Entry keys (f v)

instance Functor (Map k1 k2) where
     fmap f (Map entry) = Map (map (fmap f) entry)

test1 = show (fmap (map toUpper) $
    Map [Entry (0, 0) "origin", Entry (800, 0) "right corner"]) == show (
    Map [Entry (0, 0) "ORIGIN", Entry (800, 0) "RIGHT CORNER"])

