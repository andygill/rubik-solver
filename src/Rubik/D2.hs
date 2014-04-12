module Rubik.D2 where

import Data.Ix

import Rubik.Reverse as R
import Rubik.Vector  as V

-- http://en.wikipedia.org/wiki/Cartesian_coordinate_system
data D2 = X | Y 
   deriving (Eq,Ord,Show,Enum,Ix)

-- (clockwise) turn
turn :: Reverse dir => Vector D2 dir -> Vector D2 dir
turn (Vector X dir) = Vector Y (R.reverse dir)
turn (Vector Y dir) = Vector X dir
