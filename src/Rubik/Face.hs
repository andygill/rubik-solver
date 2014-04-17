{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Rubik.Face where

import Prelude hiding (Left, Right)

import Data.Array as A
import Control.Applicative

import Rubik.Map        as M
import Rubik.Turn       as T
import Rubik.Reverse    as R

data File = Left | Center | Right
    deriving (Eq,Ord,Enum,Show,Ix)

instance Reverse File where
    reverse Left   = Right
    reverse Center = Center
    reverse Right  = Right

data Rank = Top | Middle | Bottom
    deriving (Eq,Ord,Enum,Show,Ix)

data Square = Square Rank File
    deriving (Eq,Ord,Ix)
                
instance Show Square where
    show (Square r f) = [ head (show r) , head (show f) ]

corners :: (Square,Square)
corners = (Square Top Left,Square Bottom Right)

squares :: [[Square]]
squares = [ [ Square a b | b <- [Left .. Right] ] |  a <- [Top .. Bottom]]

instance Key Square where
  universe = concat squares

type Face a = M.Map Square a

instance Show a => Show (M.Map Square a) where
   show f = unlines $
                   bar ++
		   concat
	           [ [ "|" ++ concat [ show' (f M.! (Square rank file)) ++ "|" | file <- [Left  .. Right] ] ] ++ bar
		     | rank <- [Top .. Bottom ]
		   ]
    where maxWidth = maximum $ map (length . show) $ map (f M.!) $ concat $ squares
          show' n  = take maxWidth (show n ++ repeat ' ')
          bar      = [ "+" ++ concat [ take maxWidth (repeat '-') ++ "+" | _ <- [1..3::Int]] ]

-------------------------------------


facePlacement :: Face (Int,Int)
facePlacement = f <$> coord
  where f (Square a b) = (file b,rank a)
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


rotateBy :: Turn -> Square -> Square
rotateBy NoTurn       = id
rotateBy Clock        = clockwise
rotateBy OneEighty    = clockwise . clockwise
rotateBy CounterClock = clockwise . clockwise . clockwise
