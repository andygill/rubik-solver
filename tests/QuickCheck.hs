module Main where

import Test.QuickCheck

import Rubik.Reverse as R
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
    


-- D3
prop_1_turnD3 :: Axis D3 -> Axis D3 -> Bool
prop_1_turnD3 a1 a2
         | a1 == a2  || a1 == R.reverse a2 = turnD3 a1 a2 == a2
         | otherwise = turnD3 a1 a2 /= a2

-- turn twice reverses sign
prop_2_turnD3 :: Axis D3 -> Axis D3 -> Property
prop_2_turnD3 a1 a2 = (a1 /= a2  && a1 /= R.reverse a2) ==> turnD3 a1 (turnD3 a1 a2) == R.reverse a2

-- turn four times returns to original vector
prop_3_turnD3 :: Axis D3 -> Axis D3 -> Bool
prop_3_turnD3 a1 a2 = turnD3 a1 (turnD3 a1 (turnD3 a1 (turnD3 a1 a2))) == a2

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

instance Arbitrary Sign where
    arbitrary = elements universe

instance Arbitrary Turn where
    arbitrary = elements universe
    