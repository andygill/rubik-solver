module Rubik.Color where

import Data.Array(Ix)
import Data.Char(toLower)

import Rubik.Key
import Rubik.Cube
import Rubik.Sign
import Rubik.Axis

import Rubik.D3

data Color = Red | Orange | White | Blue | Green | Yellow
		deriving (Eq,Ord,Enum,Show,Ix)

instance Key Color where
    universe = [ Red, Orange, White, Blue, Green, Yellow ]

-- to HTML5 color
showColor :: Color -> String
showColor = map toLower . show

start :: Cube Color
start = f
  where f (Axis Z Plus)  = Orange
        f (Axis Z Minus) = Red
        f (Axis Y Plus)  = Yellow
        f (Axis Y Minus) = White
        f (Axis X Plus)  = Blue 
        f (Axis X Minus) = Green



