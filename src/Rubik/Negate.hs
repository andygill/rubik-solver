module Rubik.Negate where
 
import Prelude hiding (negate)
import qualified Prelude as P


class Negate a where
  negate :: a -> a

instance Negate Integer where
  negate = P.negate


prop_negate :: (Eq a, Negate a) => a -> Bool
prop_negate a = negate (negate a) == a

