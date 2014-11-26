module Rubik.Turn where
        
import Data.Array
import Rubik.Key

data Turn = NoTurn | Clock | OneEighty | CounterClock
   deriving (Eq,Ord,Show,Enum,Ix)

instance Num Turn where
  x + y = fromInteger ((turnToInteger x + turnToInteger y) `mod` 4)
  x - y = fromInteger ((turnToInteger x - turnToInteger y) `mod` 4)
  x * y = fromInteger ((turnToInteger x * turnToInteger y) `mod` 4)
  negate x = fromInteger (negate (turnToInteger x) `mod` 4)
  abs x    = fromInteger (abs    (turnToInteger x) `mod` 4)
  signum x = fromInteger (signum (turnToInteger x) `mod` 4)
  
  fromInteger 0 = NoTurn
  fromInteger 1 = Clock
  fromInteger 2 = OneEighty
  fromInteger 3 = CounterClock

turnToInteger NoTurn       = 0
turnToInteger Clock        = 1
turnToInteger OneEighty    = 2
turnToInteger CounterClock = 3

class Rotate a where
  -- can complete either
  turn :: a -> a
  turn = rotateBy Clock

  rotateBy :: Turn -> a -> a
  rotateBy Clock        = turn
  rotateBy OneEighty    = turn . turn
  rotateBy CounterClock = turn . turn . turn

-- We invert the rotate because it is the co-varient position
instance Rotate a => Rotate (a -> b) where
  rotateBy t f a = f (rotateBy (-t) a)
  
  
instance (Rotate a,Rotate b) => Rotate (a,b) where
    rotateBy t (a,b) = (rotateBy t a, rotateBy t b)

instance Key Turn where
     universe = [ NoTurn, Clock, OneEighty, CounterClock ]
  
data Apply a b = Apply (a -> b) a

apply :: Apply a b -> b
apply (Apply f a) = f a
 
instance Rotate a => Rotate (Apply a b) where
  turn (Apply f a) = Apply f (turn a)

