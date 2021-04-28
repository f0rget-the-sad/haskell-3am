{- Functor from Monad
 -
Prelude> :t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
Prelude> :t (fmap)
(fmap) :: Functor f => (a -> b) -> f a -> f b
Prelude> :t return
return :: Monad m => a -> m a

fmap (a -> b) -> SomeType a -> SomeType b
To get SomeType b from SomeType b we have bind, so just use it and pack
result of the function

instance Functor SomeType where
    fmap f x = x >>= (\a -> return (f a))
-}
