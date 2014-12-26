{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
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


instance Rotate Turn3D (Puzzle a) where
  rotate twist (Puzzle p) = Puzzle $ \ sd sq -> case megaAxis (rotate (N.negate twist) (mega sd sq)) of
                                                  (sd',sq') -> p sd' sq'

data Layer = E Sign      -- -2 or 2
           | I Abs       -- -1 | 0 | 1
           deriving (Eq, Ord, Show)

instance Key Layer where
    universe = map E universe ++ map I universe

instance Negate Layer where
    negate (E s) = E (N.negate s)
    negate (I a) = I (N.negate a)

type Mega = V3 Layer -- TODO: Rename

mega :: Axis D3 -> V2 Abs -> Mega
mega (Axis X s) (V2 y z) = V3 (E s) (I y) (I z)
mega (Axis Y s) (V2 x z) = V3 (I x) (E s) (I z)
mega (Axis Z s) (V2 x y) = V3 (I x) (I y) (E s)

megaAxis :: Mega -> (Axis D3,V2 Abs)
megaAxis (V3 (E s) (I y) (I z)) = (Axis X s,V2 y z)
megaAxis (V3 (I x) (E s) (I z)) = (Axis Y s,V2 x z)
megaAxis (V3 (I x) (I y) (E s)) = (Axis Z s,V2 x y)
