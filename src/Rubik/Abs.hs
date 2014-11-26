module Rubik.Abs where

import Data.Ix

import Rubik.Reverse as R
import Rubik.Key

data Abs = MinusOne | Zero | PlusOne
    deriving (Eq,Ord,Enum,Ix,Bounded)

instance Show Abs where
    show MinusOne = "-1"
    show Zero     = "0"
    show PlusOne  = "+1"

instance R.Reverse Abs where
    reverse MinusOne = PlusOne
    reverse Zero  = Zero
    reverse PlusOne  = MinusOne

instance Key Abs where
    universe = [ MinusOne, Zero, PlusOne ]
