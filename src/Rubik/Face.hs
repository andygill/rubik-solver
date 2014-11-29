{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Rubik.Face where

import Prelude hiding (Left, Right)

import Data.Array as A
import Control.Applicative

import Rubik.Key        as K
import Rubik.Turn       as T
import Rubik.Negate     as N
import Rubik.V2       
import Rubik.Abs

{-
data File = Left | Center | Right
    deriving (Eq,Ord,Enum,Show,Ix)

instance Negate File where
    negate Left   = Right
    negate Center = Center
    negate Right  = Right

instance Key File where
    universe = [Left, Center, Right]

data Rank = Top | Middle | Bottom
    deriving (Eq,Ord,Enum,Show,Ix)

instance Negate Rank where
    negate Top    = Bottom
    negate Middle = Middle
    negate Bottom = Top
    
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
-}

type Face a = V2 Abs -> a

{-
facePlacement :: Face (Int,Int)
facePlacement (V2 a b) = (f a, f (N.negate b))
  where
        f MinusOne = 0
        f Zero     = 1
        f PlusOne  = 2
-}

                           
