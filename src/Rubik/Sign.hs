module Rubik.Sign where

import Data.Ix

import Rubik.Reverse as R


data Sign = Minus | Plus
    deriving (Eq,Ord,Enum,Ix)

instance Show Sign where
    show Minus = "-"
    show Plus  = "+"

instance R.Reverse Sign where
    reverse Minus = Plus
    reverse Plus  = Minus

mulSign :: Sign -> Sign -> Sign
mulSign x y | x == y    = Plus
            | otherwise = Minus