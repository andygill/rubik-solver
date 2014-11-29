module Rubik.Sign where

import Data.Ix

import Rubik.Negate as N
import Rubik.Key

data Sign = Minus | Plus
    deriving (Eq,Ord,Enum,Ix)

instance Show Sign where
    show Minus = "-"
    show Plus  = "+"

instance N.Negate Sign where
    negate Minus = Plus
    negate Plus  = Minus
{-
mulSign :: Sign -> Sign -> Sign
mulSign x y | x == y    = Plus
            | otherwise = Minus
-}            
instance Key Sign where
   universe = [ Minus, Plus ]
   
               
-- This is a sort of 'scaleBy'
mulBySign :: Negate a => Sign -> a -> a
mulBySign Plus  t = t
mulBySign Minus t = N.negate t
               