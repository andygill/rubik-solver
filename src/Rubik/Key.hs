module Rubik.Key where

class {- Ord k => -} Key k where
  universe :: [k]

instance Key Bool where
  universe = [False,True]

instance (Key a, Key b) => Key (a,b) where
  universe = [ (a,b) | a <- universe, b <- universe ]

elems :: Key k => (k -> a) -> [a] 
elems f = map f universe

toList :: Key k => (k -> a) -> [(k,a)]
toList f = [ (k,f k) | k <- universe ]

newtype Finite a b = Finite (a -> b)

instance (Show a, Key a, Show b) => Show (Finite a b) where
  show (Finite f) = show [ (a,f a) | a <- universe ]          

instance Functor (Finite a) where
  fmap f (Finite g) = Finite (f . g)