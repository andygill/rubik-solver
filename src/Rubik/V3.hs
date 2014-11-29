module Rubik.V3 where
        
import Rubik.Abs
import Rubik.Axis
import Rubik.D3
import Rubik.Negate as N
import Rubik.Sign as S
import Rubik.Turn as T
import Rubik.V2
import Rubik.Key as K

data V3 a = V3 a a a
        deriving (Eq,Ord,Show)

turnV3 :: Negate a => Axis D3 -> V3 a -> V3 a
turnV3 (Axis X Plus)  (V3 x y z) = V3 x z (N.negate y)
turnV3 (Axis X Minus) (V3 x y z) = V3 x (N.negate z) y
turnV3 (Axis Y Plus)  (V3 x y z) = V3 z y (N.negate x)
turnV3 (Axis Y Minus) (V3 x y z) = V3 (N.negate z) y x
turnV3 (Axis Z Plus)  (V3 x y z) = V3 y (N.negate x) z
turnV3 (Axis Z Minus) (V3 x y z) = V3 (N.negate y) x z

instance Key a => Key (V3 a) where
    universe = [ V3 a b c | a <- universe, b <- universe, c <- universe ]
