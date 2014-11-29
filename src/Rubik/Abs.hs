module Rubik.Abs where

import Data.Ix

import Rubik.Negate as N
import Rubik.Key

data Abs = MinusOne | Zero | PlusOne
    deriving (Eq,Ord,Enum,Ix,Bounded)

instance Show Abs where
    show MinusOne = "-1"
    show Zero     = "0"
    show PlusOne  = "+1"

instance N.Negate Abs where
    negate MinusOne = PlusOne
    negate Zero  = Zero
    negate PlusOne  = MinusOne

instance Key Abs where
    universe = [ MinusOne, Zero, PlusOne ]
