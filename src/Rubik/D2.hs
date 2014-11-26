module Rubik.D2 where

import Data.Ix

import Rubik.Reverse as R
import Rubik.Axis  as V
import Rubik.Turn    as T
import Rubik.Key as K

-- http://en.wikipedia.org/wiki/Cartesian_coordinate_system
data D2 = X | Y 
   deriving (Eq,Ord,Show,Enum,Ix)

instance Key D2 where
   universe = [ X, Y ]

-- (clockwise) turn
turnD2 :: Axis D2 -> Axis D2
turnD2 (Axis X dir) = Axis Y (R.reverse dir)
turnD2 (Axis Y dir) = Axis X dir

-- 4 turns gets back to original value
prop_1_turnD2 :: Axis D2 -> Bool
prop_1_turnD2 a = turnD2 (turnD2 (turnD2 (turnD2 a))) == a

-- any single turn will not give the same value
prop_2_turnD2 :: Axis D2 -> Bool
prop_2_turnD2 a = turnD2 a /= a


