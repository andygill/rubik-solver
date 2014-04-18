module Rubik.Cube where

import Data.Array as A
import Control.Applicative

import Rubik.Key as K
import Rubik.Turn as T
import Rubik.Axis as V
import Rubik.Sign       as S
import Rubik.D3         as D3

data Side      = F | U | R | D | L | B
		deriving (Eq,Ord,Enum,Show,Ix)

sides :: [Side]
sides = [F .. B]

instance Key Side where
  universe = sides

type Cube a = Axis D3 -> a


-- Where is this side placed on a 2D plane.
cubePlacement :: Cube (Int,Int)
cubePlacement = f
  where f (Axis Z Plus)  = (1,1)
        f (Axis Z Minus) = (1,0)
        f (Axis Y Plus)  = (2,1)
        f (Axis Y Minus) = (1,2)
        f (Axis X Plus)  = (0,1)
        f (Axis X Minus) = (3,1)

{-
sideToAxis :: Side -> Axis D3
sideToAxis F = Axis D3.Z Plus
sideToAxis B = Axis D3.Z Minus
sideToAxis U = Axis D3.Y Plus
sideToAxis D = Axis D3.Y Minus
sideToAxis R = Axis D3.X Plus
sideToAxis L = Axis D3.X Minus

axisToSize :: Axis D3 -> Side
axisToSize :: Axis D3 -> Side

--clockwiseCube :: Side -> Side -> Side
--clockwiseCube view start = turnD3

--rotateBy :: F -> F -> F
-}