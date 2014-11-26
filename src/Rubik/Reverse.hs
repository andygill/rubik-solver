module Rubik.Reverse where
 
import Prelude hiding (reverse)

class Reverse a where
  reverse :: a -> a


prop_reverse :: (Eq a, Reverse a) => a -> Bool
prop_reverse a = reverse (reverse a) == a
