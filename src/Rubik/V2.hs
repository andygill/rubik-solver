module Rubik.V2 where
        
import Rubik.Reverse as R
import Rubik.Sign as S
import Rubik.Turn as T
import Rubik.D2
import Rubik.Key
        
data V2 a = V2 a a
        deriving (Eq,Ord,Show)

--lookupV2 :: V2 a -> D2 -> a
--lookupV2 (V3 x y

instance Reverse a => Rotate (V2 a) where
  turn (V2 x y) = (V2 y (R.reverse x))

instance (Key a) => Key (V2 a) where
    universe = [ V2 a b | a <- universe, b <- universe ]
