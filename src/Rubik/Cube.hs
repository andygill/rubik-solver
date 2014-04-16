module Rubik.Cube where

import Data.Array as A
import Control.Applicative

import Rubik.Map as M
import Rubik.Turn as T

data Side      = F | U | R | D | L | B
		deriving (Eq,Ord,Enum,Show,Ix)

sides :: [Side]
sides = [F .. B]

instance Key Side where
  universe = sides

type Cube a = M.Map Side a

cubePlacement :: Cube (Int,Int)
cubePlacement = f <$> coord
  where f F = (1,1)
        f U = (1,0)
        f R = (2,1)
        f D = (1,2)
        f L = (0,1)
        f B = (3,1)
