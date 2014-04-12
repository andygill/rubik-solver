module Rubik.D3 where

import Data.Ix

import Rubik.Reverse as R
import Rubik.Vector  as V

data D3 = X | Y | Z
        deriving (Eq,Ord,Show,Enum,Ix)

-- From http://en.wikipedia.org/wiki/Right-hand_rule,
-- Thumb ~ x, fore ~ y, other ~ z.
rightHandTurn :: Reverse dir => D3 -> Vector D3 dir -> Vector D3 dir
rightHandTurn X (Vector X dir) = Vector X dir
rightHandTurn X (Vector Y dir) = Vector Z (R.reverse dir)
rightHandTurn X (Vector Z dir) = Vector Y dir
rightHandTurn Y (Vector X dir) = Vector Z dir
rightHandTurn Y (Vector Y dir) = Vector Y dir
rightHandTurn Y (Vector Z dir) = Vector Z (R.reverse dir)
rightHandTurn Z (Vector X dir) = Vector Z (R.reverse dir)
rightHandTurn Z (Vector Y dir) = Vector Y dir
rightHandTurn Z (Vector Z dir) = Vector Z dir

