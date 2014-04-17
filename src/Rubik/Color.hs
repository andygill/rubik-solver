module Rubik.Color where

import Data.Array(Ix)
import Data.Char(toLower)

import Rubik.Map
import Rubik.Cube
import Rubik.Sign
import Rubik.Vector

import Rubik.D3

data Color = Red | Orange | White | Blue | Green | Yellow
		deriving (Eq,Ord,Enum,Show,Ix)

showColor :: Color -> String
showColor = map toLower . show

-- F | U | R | D | L | B
start :: Cube Color
start = mkMap f
  where f (Axis Z Plus)  = Green
        f (Axis Z Minus) = White
        f (Axis Y Plus)  = Red
        f (Axis Y Minus) = Yellow
        f (Axis X Plus)  = Orange
        f (Axis X Minus) = Blue

