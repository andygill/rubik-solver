{-# LANGUAGE MultiParamTypeClasses #-}
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

data Twist = Twist Turn D3

instance Negate a => Rotate Twist (V3 a) where
  rotate (Twist t X) (V3 x y z) = V3 x y' z' where (V2 y' z') = rotate t $ V2 y z
  rotate (Twist t Y) (V3 x y z) = V3 x' y z' where (V2 x' z') = rotate t $ V2 x z
  rotate (Twist t Z) (V3 x y z) = V3 x' y' z where (V2 x' y') = rotate t $ V2 x y

instance Key a => Key (V3 a) where
    universe = [ V3 a b c | a <- universe, b <- universe, c <- universe ]

