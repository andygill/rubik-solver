module Rubik.Turn where
        
import Data.Array

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
