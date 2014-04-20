{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Rubik.Face where

import Prelude hiding (Left, Right)

import Data.Array as A
import Control.Applicative

import Rubik.Key        as K
import Rubik.Turn       as T
import Rubik.Reverse    as R

data File = Left | Center | Right
    deriving (Eq,Ord,Enum,Show,Ix)

instance Reverse File where
    reverse Left   = Right
    reverse Center = Center
    reverse Right  = Right

instance Key File where
    universe = [Left, Center, Right]

data Rank = Top | Middle | Bottom
    deriving (Eq,Ord,Enum,Show,Ix)

instance Reverse Rank where
    reverse Top    = Bottom
    reverse Middle = Middle
    reverse Bottom = Top
    
instance Key Rank where
    universe = [Top, Middle, Bottom]

data Square = Square Rank File
    deriving (Eq,Ord,Ix)
                
instance Show Square where
    show (Square r f) = [ head (show r) , head (show f) ]

instance Key Square where
    universe = [ Square r f | r <- universe, f <- universe ]

corners :: (Square,Square)
corners = (Square Top Left,Square Bottom Right)

squares :: [[Square]]
squares = [ [ Square a b | b <- [Left .. Right] ] |  a <- [Top .. Bottom]]

type Face a = Square -> a

facePlacement :: Face (Int,Int)
facePlacement (Square a b) = (file b,rank a)
  where
        rank Top    = 0
        rank Middle = 1
        rank Bottom = 2
        file Left   = 0
        file Center = 1
        file Right  = 2
        

type Permutation a = a -> a

clockwise :: Permutation Square
clockwise (Square r f) = Square (case f of
                                   Left   -> Top
                                   Center -> Middle
                                   Right  -> Bottom)
                                (case r of
                                   Top    -> Right
                                   Middle -> Center
                                   Bottom -> Left)


{-
                                   
rotateBy :: Turn -> Face a -> Face a
rotateBy NoTurn       f = f
rotateBy Clock        f = f . clockwise . clockwise . clockwise
--rotateBy OneEighty    = clockwise . clockwise
--rotateBy CounterClock = 

-}

instance Rotate Square where
  rotateBy NoTurn       = id
  rotateBy Clock        = clockwise
  rotateBy OneEighty    = clockwise . clockwise
  rotateBy CounterClock = clockwise . clockwise . clockwise
          
                           
