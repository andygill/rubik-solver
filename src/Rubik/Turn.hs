{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}
module Rubik.Turn where
        
import Data.Array
import Rubik.Negate as N
import Rubik.Key

data Turn = NoTurn | Clock | OneEighty | CounterClock
   deriving (Eq,Ord,Show,Enum,Ix)

instance Negate Turn where
    negate NoTurn       = NoTurn
    negate Clock        = CounterClock
    negate OneEighty    = OneEighty
    negate CounterClock = Clock

instance Key Turn where
     universe = [ NoTurn, Clock, OneEighty, CounterClock ]

class Rotate a where
  type SideOf a
  rotate :: SideOf a -> a -> a

-- never used
--instance (Negate a, Rotate a b) => Rotate a (b -> c) where
--  rotate t f a = f (rotate (N.negate t) a)

{-
-- Split into its own module
class Rotate a where
  type SideOf a
   
  rotate :: SideOf a -> a -> a

  -- can complete either
  turn :: a -> a
  turn = rotateBy Clock

  rotateBy :: Turn -> a -> a
  rotateBy Clock        = turn
  rotateBy OneEighty    = turn . turn
  rotateBy CounterClock = turn . turn . turn

-- We invert the rotate because it is the co-varient position
instance Rotate a => Rotate (a -> b) where
  type SideOf (a -> b) = SideOf a
  rotateBy t f a = f (rotateBy (N.negate t) a)
  
instance (Rotate a,Rotate b) => Rotate (a,b) where
    rotateBy t (a,b) = (rotateBy t a, rotateBy t b)

  
data Apply a b = Apply (a -> b) a

apply :: Apply a b -> b
apply (Apply f a) = f a
 
instance Rotate a => Rotate (Apply a b) where
  turn (Apply f a) = Apply f (turn a)

-}