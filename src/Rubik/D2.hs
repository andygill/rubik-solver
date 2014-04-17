module Rubik.D2 where

import Data.Ix

import Rubik.Reverse as R
import Rubik.Axis  as V
import Rubik.Turn    as T

-- http://en.wikipedia.org/wiki/Cartesian_coordinate_system
data D2 = X | Y 
   deriving (Eq,Ord,Show,Enum,Ix)

-- (clockwise) turn
turnD2 :: Axis D2 -> Axis D2
turnD2 (Axis X dir) = Axis Y (R.reverse dir)
turnD2 (Axis Y dir) = Axis X dir

