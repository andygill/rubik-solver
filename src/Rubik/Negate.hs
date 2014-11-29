module Rubik.Negate where
 
import Prelude hiding (negate)

class Negate a where
  negate :: a -> a


prop_negate :: (Eq a, Negate a) => a -> Bool
prop_negate a = negate (negate a) == a

