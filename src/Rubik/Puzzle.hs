{-# LANGUAGE OverloadedStrings #-}
module Rubik.Puzzle where
        
import Control.Applicative

import Data.Text(Text,pack)

import Rubik.Shape
import Rubik.Face
import Rubik.Turn
import Rubik.D3
import Rubik.Cube
import Rubik.Color
import Rubik.Axis
import Rubik.Sign
import Rubik.Key
import Rubik.V2
import Rubik.V3
import Rubik.Abs
import Rubik.Negate as N

-- Not abstact, Just boxed in a newtype for instances.

newtype Puzzle a = Puzzle { unPuzzle :: Cube (Face a) }

instance Show a => Show (Puzzle a) where
  show (Puzzle p) = show (Finite (fmap Finite p))

instance Functor Puzzle where
  fmap f (Puzzle p) = Puzzle $ fmap (fmap f) $ p


instance Applicative Puzzle where
  pure = Puzzle . pure . pure
  (Puzzle f) <*> (Puzzle a) = Puzzle $ liftA2 (liftA2 ($)) f a

faceMap :: (Face a -> Face b) -> Puzzle a -> Puzzle b
faceMap f (Puzzle p) = Puzzle (fmap f p)

--foldPuzzle :: (Face a -> b) -> Puzzle a -> Cube b

puzzleSide   :: Puzzle (Axis D3)        -- Puzzle Side
puzzleSide = Puzzle (\ s _ -> s)

puzzleSquare :: Puzzle (V2 Abs)         -- Puzzle location
puzzleSquare = Puzzle (\ _ sq -> sq)

