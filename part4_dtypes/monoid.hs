import Data.Monoid
import Data.List (find)
import Prelude hiding (lookup)
import qualified Data.List as L

newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

instance Semigroup Xor where
    a <> b = Xor (not(a == b))

instance Monoid Xor where
    mempty = Xor False
    -- mappend = undefined


class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

instance MapLike ListMap where
    empty = ListMap []
    lookup k (ListMap lp) = case find (\(key, _) -> key == k) lp of
        Nothing -> Nothing
        Just (k,v) -> Just v
    insert k v lmo@(ListMap lp) = case lookup k lmo of
        Nothing -> ListMap ((k, v) : lp)
        Just _ -> ListMap (map (\p@(key, val) -> if key == k then (k, v) else p) lp)
    delete k (ListMap lp) = ListMap (filter (\(key, _) -> key /= k) lp)

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
  empty = ArrowMap $ const Nothing
  lookup key (ArrowMap map) = map key
  insert key value (ArrowMap map) =
    ArrowMap (\k -> if k == key then Just value else map k)
  delete key (ArrowMap map) =
    ArrowMap (\k -> if k == key then Nothing else map k)
