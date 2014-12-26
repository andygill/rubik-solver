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

data Turn3D = Turn3D Turn D3

instance Negate Turn3D where
  negate (Turn3D t d) = (Turn3D (N.negate t) d)

instance Negate a => Rotate Turn3D (V3 a) where
  rotate (Turn3D t X) (V3 x y z) = V3 x  y' z' where (V2 y' z') = rotate t $ V2 y z
  rotate (Turn3D t Y) (V3 x y z) = V3 x' y  z' where (V2 z' x') = rotate t $ V2 z x -- intentually flipped
  rotate (Turn3D t Z) (V3 x y z) = V3 x' y' z  where (V2 x' y') = rotate t $ V2 x y

instance Key a => Key (V3 a) where
    universe = [ V3 a b c | a <- universe, b <- universe, c <- universe ]

