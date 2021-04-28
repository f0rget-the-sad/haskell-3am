module Reader where

import Control.Monad (ap, liftM)

data Reader r a = Reader { runReader :: (r -> a) }

instance Functor (Reader r) where
    fmap = liftM

instance Applicative (Reader r) where
    pure = return
    (<*>) = ap

instance Monad (Reader r) where
  return x = Reader $ \_ -> x
  m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r

ask :: Reader r r
ask = Reader id

asks :: (r -> a) -> Reader r a
asks = Reader

local' :: (r -> r') -> Reader r' a -> Reader r a
local' f m = Reader $ \e -> runReader m (f e)

type User = String
type Password = String
type UsersTable = [(User, Password)]

usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = do
    lst <- ask
    return [u | (u, p) <- lst, p == "123456"]
