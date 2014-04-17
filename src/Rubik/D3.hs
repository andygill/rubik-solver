module Rubik.D3 where

import Data.Ix

import Rubik.Reverse as R
import Rubik.Vector  as V
import Rubik.Sign    as S

data D3 = X | Y | Z
        deriving (Eq,Ord,Show,Enum,Ix)

-- From http://en.wikipedia.org/wiki/Right-hand_rule,
-- Thumb ~ x, fore ~ y, other ~ z.

-- (This is a cross-product, where the first argument is +1, I think)
turnD3 :: Vector D3 -> Vector D3 -> Maybe (Vector D3)
turnD3 (Vector a1 d1) (Vector a2 d2) 
        | a1 == a2           = Nothing
        | a1 == X && a2 == Y = return $ Vector Z $ R.reverse d 
        | a1 == X && a2 == Z = return $ Vector Y $ d
        | a1 == Y && a2 == X = return $ Vector Z $ d
        | a1 == Y && a2 == Z = return $ Vector X $ R.reverse d
        | a1 == Z && a2 == X = return $ Vector Y $ R.reverse d
        | a1 == Z && a2 == Y = return $ Vector X $ d
  where d = d1 `mulSign` d2

