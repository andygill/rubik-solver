module Rubik.V3 where
        
import Rubik.Abs
import Rubik.Axis
import Rubik.D3
import Rubik.Reverse as R
import Rubik.Sign as S
import Rubik.Turn as T
import Rubik.V2
import Rubik.Key as K

data V3 a = V3 a a a
        deriving (Eq,Ord,Show)

turnV3 :: Reverse a => Axis D3 -> V3 a -> V3 a
turnV3 (Axis X Plus)  (V3 x y z) = V3 x z (R.reverse y)
turnV3 (Axis X Minus) (V3 x y z) = V3 x (R.reverse z) y
turnV3 (Axis Y Plus)  (V3 x y z) = V3 z y (R.reverse x)
turnV3 (Axis Y Minus) (V3 x y z) = V3 (R.reverse z) y x
turnV3 (Axis Z Plus)  (V3 x y z) = V3 y (R.reverse x) z
turnV3 (Axis Z Minus) (V3 x y z) = V3 (R.reverse y) x z

instance Key a => Key (V3 a) where
    universe = [ V3 a b c | a <- universe, b <- universe, c <- universe ]
