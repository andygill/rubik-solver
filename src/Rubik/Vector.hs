module Rubik.Vector where

import Data.Ix

import Rubik.Reverse    as R
import Rubik.Turn       as T

-- Perhaps call Axis, or Unit.
data Vector dim dir = Vector dim dir
    deriving (Eq,Ord,Ix, Show)

instance Reverse dir => Reverse (Vector dim dir) where
    reverse (Vector dim dir) = Vector dim (R.reverse dir)
