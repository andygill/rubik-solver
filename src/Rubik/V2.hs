{-# LANGUAGE TypeFamilies #-}
module Rubik.V2 where
        
import Rubik.Reverse as R
import Rubik.Sign as S
import Rubik.Turn as T
import Rubik.D2
import Rubik.Key
import Rubik.Abs
        
data V2 a = V2 a a
        deriving (Eq,Ord,Show)

--lookupV2 :: V2 a -> D2 -> a
--lookupV2 (V3 x y

instance Reverse a => Rotate (V2 a) where
  type SideOf (V2 a) = Sign
  rotate Plus  (V2 x y) = V2 y (R.reverse x)
  rotate Minus (V2 x y) = V2 (R.reverse y) x

instance (Key a) => Key (V2 a) where
    universe = [ V2 a b | a <- universe, b <- universe ]

-- rotate and rotate back is an identity
prop_rotate_V2 :: Sign -> V2 Abs -> Bool
prop_rotate_V2 sgn v2 = rotate (R.reverse sgn) (rotate sgn v2) == v2
