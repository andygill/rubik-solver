module Rubik.Color where

import Data.Array(Ix)
import Data.Char(toLower)

import Rubik.Map
import Rubik.Cube

import Rubik.D3

data Color = Red | Orange | White | Blue | Green | Yellow
		deriving (Eq,Ord,Enum,Show,Ix)

showColor :: Color -> String
showColor = map toLower . show

-- F | U | R | D | L | B
start :: Cube Color
start = mkMap f
  where f F = Green
        f U = White
        f R = Red
        f D = Yellow
        f L = Orange
        f B = Blue

