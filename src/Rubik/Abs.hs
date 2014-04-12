module Rubik.Abs where

import Data.Ix

import Rubik.Reverse as R

data Abs = Minus | Zero | Plus
    deriving (Eq,Ord,Enum,Ix)

instance Show Abs where
    show Minus = "-"
    show Zero  = "0"
    show Plus  = "+"

instance R.Reverse Abs where
    reverse Minus = Plus
    reverse Zero  = Zero
    reverse Plus  = Minus
