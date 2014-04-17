module Rubik.D2 where

import Data.Ix

import Rubik.Reverse as R
import Rubik.Vector  as V
import Rubik.Turn    as T

-- http://en.wikipedia.org/wiki/Cartesian_coordinate_system
data D2 = X | Y 
   deriving (Eq,Ord,Show,Enum,Ix)

-- (clockwise) turn
turnD2 :: Vector D2 -> Vector D2
turnD2 (Vector X dir) = Vector Y (R.reverse dir)
turnD2 (Vector Y dir) = Vector X dir

