module Rubik.Vector where

import Data.Ix

import Rubik.Reverse    as R
import Rubik.Sign       as S

-- Perhaps call Axis, or Unit.
data Vector dim = Vector dim Sign
    deriving (Eq,Ord,Ix, Show)

instance Reverse (Vector dim) where
    reverse (Vector dim dir) = Vector dim (R.reverse dir)
