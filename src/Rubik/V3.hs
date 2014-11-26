module Rubik.V3 where
        
import Rubik.Reverse as R
import Rubik.Sign as S
import Rubik.Turn as T
import Rubik.V2
        
data V3 a = V3 a a a
        deriving (Eq,Ord,Show)

data V3slice a 
   = V3X a (V2 a)
   | V3Y a (V2 a)
   | V3Z a (V2 a)


--instance Reverse a => Rotate (V2 a) where
--  turn (V2 x y) = (V2 y (R.reverse x))

