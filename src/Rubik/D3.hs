module Rubik.D3 where

import Data.Ix

import Rubik.Reverse as R
import Rubik.Vector  as V
import Rubik.Sign    as S
import Rubik.Map     as M

data D3 = X | Y | Z
        deriving (Eq,Ord,Show,Enum,Ix)

instance Key D3 where
   universe = [ X, Y, Z ]

-- From http://en.wikipedia.org/wiki/Right-hand_rule,
-- Thumb ~ x, fore ~ y, other ~ z.

turnD3 :: Axis D3 -> Axis D3 -> Maybe (Axis D3)
turnD3 (Axis a1 d1) (Axis a2 d2) 
        | a1 == a2           = Nothing
        | a1 == X && a2 == Y = return $ Axis Z $ R.reverse d 
        | a1 == X && a2 == Z = return $ Axis Y $ d
        | a1 == Y && a2 == X = return $ Axis Z $ d
        | a1 == Y && a2 == Z = return $ Axis X $ R.reverse d
        | a1 == Z && a2 == X = return $ Axis Y $ R.reverse d
        | a1 == Z && a2 == Y = return $ Axis X $ d
  where d = d1 `mulSign` d2
