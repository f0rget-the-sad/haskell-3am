class (Bounded a, Eq a, Enum a) => SafeEnum a where
  ssucc :: a -> a
  ssucc a = if a == maxBound then minBound else succ a

  spred :: a -> a
  spred a = if a == minBound then maxBound else pred a