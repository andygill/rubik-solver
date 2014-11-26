{-# LANGUAGE FlexibleInstances #-}
module Rubik.D3 where

import Data.Ix

import Rubik.Reverse as R
import Rubik.Axis  as V
import Rubik.Sign    as S
import Rubik.Key     as K
import Rubik.Turn    as T

data D3 = X | Y | Z
        deriving (Eq,Ord,Show,Enum,Ix,Bounded)

instance Key D3 where
   universe = [ X, Y, Z ]

-- From http://en.wikipedia.org/wiki/Right-hand_rule,
-- Thumb ~ z, fore ~ y, other ~ x. (This means that z is +ve towards you.)

-- From the point of the first argument, turn the second argument clockwise.
turnD3 :: Axis D3 -> Axis D3 -> Axis D3
turnD3 (Axis a1 d1) (Axis a2 d2) 
        | a1 == a2           = Axis a2 d2
        | a1 == X && a2 == Y = Axis Z $ d
        | a1 == X && a2 == Z = Axis Y $ R.reverse d 
        | a1 == Y && a2 == X = Axis Z $ R.reverse d
        | a1 == Y && a2 == Z = Axis X $ d
        | a1 == Z && a2 == X = Axis Y $ d
        | a1 == Z && a2 == Y = Axis X $ R.reverse d
  where d = d1 `mulSign` d2


class Twist a where
  -- pick a specific direction, a which way to turn, and rotate, please.
  rotateD3 :: Axis D3 -> Turn -> a -> a

instance Twist r => Twist (r -> a) where
  rotateD3 d r f a = f (rotateD3 d (-r) a)

instance Twist (Axis D3) where
  rotateD3 d NoTurn       = id
  rotateD3 d Clock        = turnD3 d 
  rotateD3 d OneEighty    = turnD3 d . turnD3 d 
  rotateD3 d CounterClock = turnD3 d . turnD3 d . turnD3 d 

instance (Twist a, Twist b) => Twist (a,b) where
  rotateD3 d t (a,b) = (rotateD3 d t a, rotateD3 d t b)
  