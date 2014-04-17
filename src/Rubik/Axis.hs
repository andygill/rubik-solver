module Rubik.Axis where

import Data.Ix

import Rubik.Reverse    as R
import Rubik.Sign       as S
import Rubik.Map        as M

-- Perhaps call Axis, or Unit.
data Axis dim = Axis dim Sign
    deriving (Eq,Ord,Ix, Show)

instance Reverse (Axis dim) where
    reverse (Axis dim dir) = Axis dim (R.reverse dir)


instance Key d => Key (Axis d) where
   universe = [ Axis d s | d <- universe, s <- universe ]
   