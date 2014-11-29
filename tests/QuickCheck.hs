module Main where

import Test.QuickCheck
import Control.Monad

import Rubik.Negate as N
import Rubik.Abs
import Rubik.Axis
import qualified Rubik.D2 as D2
import Rubik.D2 (D2)
import qualified Rubik.D3 as D3
import Rubik.D3 (D3,turnD3)
import Rubik.Key
import qualified Rubik.Sign as Sign
import Rubik.Sign (Sign)
import qualified Rubik.Turn as Turn
import Rubik.Turn (Turn)
import Rubik.V2

main = do
    -- Reverse
    quickCheck (prop_reverse :: Abs -> Bool)
    quickCheck (prop_reverse :: Sign -> Bool)
    -- D2
    quickCheck D2.prop_1_turnD2
    quickCheck D2.prop_2_turnD2
    -- D3
    quickCheck prop_1_turnD3
    quickCheck prop_2_turnD3
    quickCheck prop_3_turnD3
    quickCheck prop_4_turnD3
    quickCheck prop_5_turnD3
    -- V2
    quickCheck prop_rotate_V2 

-- D3
prop_1_turnD3 :: Axis D3 -> Axis D3 -> Bool
prop_1_turnD3 a1 a2
         | a1 == a2  || a1 == N.negate a2 = turnD3 a1 a2 == a2
         | otherwise = turnD3 a1 a2 /= a2

-- turn twice reverses sign
prop_2_turnD3 :: Axis D3 -> Axis D3 -> Property
prop_2_turnD3 a1 a2 = (a1 /= a2  && a1 /= N.negate a2) ==> turnD3 a1 (turnD3 a1 a2) == N.negate a2

-- turn four times returns to original vector
prop_3_turnD3 :: Axis D3 -> Axis D3 -> Bool
prop_3_turnD3 a1 a2 = turnD3 a1 (turnD3 a1 (turnD3 a1 (turnD3 a1 a2))) == a2

prop_4_turnD3 :: Axis D3 -> Axis D3 -> Property
prop_4_turnD3 a1 a2 = (a1 /= a2  && a1 /= N.negate a2) 
        ==> turnD3 a1 (turnD3 a1 a2) == N.negate (turnD3 (N.negate a1) (turnD3 a1 a2))

prop_5_turnD3 :: Axis D3 -> Axis D3 -> Axis D3 -> Property
prop_5_turnD3 a1 a2 a3 = (diff_axis a1 a3 && diff_axis a2 a3 && diff_axis a1 a2)
        ==> turnD3 a1 (turnD3 a2 a3) /= turnD3 a2 (turnD3 a1 a3)

diff_axis :: Axis D3 -> Axis D3 -> Bool
diff_axis a1 a2 = a1 /= a2  && a1 /= N.negate a2

-- The instances
instance Arbitrary Abs where
    arbitrary = elements [minBound .. maxBound]

instance Arbitrary d => Arbitrary (Axis d) where
    arbitrary = do
        dim <- arbitrary  
        sgn <- arbitrary
        return $ Axis dim sgn
  
instance Arbitrary D2 where
    arbitrary = elements universe

instance Arbitrary D3 where
    arbitrary = elements universe

instance Arbitrary a => Arbitrary (V2 a) where
    arbitrary = liftM2 V2 arbitrary arbitrary

instance Arbitrary Sign where
    arbitrary = elements universe

instance Arbitrary Turn where
    arbitrary = elements universe
    