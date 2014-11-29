{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Rubik.Test where
        
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
import Rubik.Puzzle


drawCube :: Puzzle Shape -> Shape
drawCube = borderShape 0.1
         . drawMap cubePlacement
         . fmap (borderShape 0.01)
         . drawFace


-- fold a puzzle into a cube of shapes
drawFace :: Puzzle Shape -> Cube Shape
drawFace (Puzzle pz) sd 
                = background "#202020" 0.01 0.1
                $ drawMap (facePlacement sd)
                $ fmap (borderShape 0.05)
                $ pz sd

facePlacement :: Cube (Face (Int, Int))
facePlacement ax sq@(V2 x0 y0) = case ax of
        (Axis X Plus)  -> (ny,nx)
        (Axis X Minus) -> (y,nx)
        (Axis Y Plus)  -> (x,y)
        (Axis Y Minus) -> (x,ny)
        (Axis Z Plus)  -> (x,ny)
        (Axis Z Minus) -> (nx,ny)
  where
        nx = loc $ N.negate x0
        ny = loc $ N.negate y0
        x = loc x0
        y = loc y0
        loc MinusOne = 0
        loc Zero     = 1
        loc PlusOne  = 2

{-
drawFace' :: Cube (Face Shape -> Shape)
drawFace' sd face = background "#202020" 0.01 0.1
                       $ drawMap (facePlacement'' sd) $ 
                           fmap (borderShape 0.05) $ face

drawCube :: Cube Shape -> Shape
drawCube = borderShape 0.1
         . drawMap cubePlacement
         . fmap (borderShape 0.01)

-}
face' :: Text -> Face Shape
face' col k = tile col `overlay` (text 5 $ show k) `overlay` (case k of
                                                                V2 PlusOne PlusOne  -> triangle
                                                                V2 PlusOne Zero     -> circle
                                                                _ -> emptyShape)

cube :: Puzzle Shape
cube = f <$> rubik <*> puzzleSide <*> puzzleSquare
  where f col sd sq = tile (pack $ showColor $ col) 
                `overlay`
                       text 7 (s (mega sd sq))
        s (V3 a b c) = show a ++ "\n" ++ show b ++ "\n" ++ show c

rubik :: Puzzle Color
rubik = Puzzle (fmap pure start)

--- blank cube
cube' :: Puzzle Shape
cube' = Puzzle
      $ fmap face'
      $ fmap (pack . showColor)
      $ start

--main = shape (drawFace $ face)
--main = shape (drawFace $ rotateBy Clock $ face "red")

--main = shape (drawCube $ fmap drawFace cube)

main = shape $ packShapes 
             [ [ drawCube $ cube ]
             , [ drawCube $ rotate (Turn3D t Z) $ cube | t <- universe ]
             ]

{-
             -- notations
z :: Cube (Face a) -> Cube (Face a)
z c0 = (\ (f,o) m -> {-rotateBy (toTurn o m)-} f) <$> c2 <*> w
  where
        c1 = (,) <$> c0 <*> cubeFaceDir
        f a b _ = a
        c2 = rotateD3 o t c1
        t =  Clock
        o = Axis X Plus
        w = fmap (rotateD3 o t) cubeFaceDir

t r o = fmap (rotateD3 o r) (cubeFaceDir)

-- First argument is the orentation of turn, then second is the face to look up
f = \ r o p -> toTurn (cubeFaceDir p) (t r o p)
--f o p = toTurn (cubeFaceDir p) (t o)

--toTurn :: (Axis D3,Axis D3) -> (Axis D3,Axis D3) -> 
toTurn (Axis d1 s1, Axis d2 s2) (Axis d3 s3, Axis d4 s4)
        | d1 == d2 || d3 == d4 = error "internal error: Axis overlap in toTurn"
        | d1 == d3 && d2 == d4 = same (s1 `mulSign` s3,s2 `mulSign` s4)
        | d1 == d4 && d2 == d3 = swapped (s1,s2) (s3,s4)
        | otherwise            = error $        "internal error: Axis mismatch in toTurn" ++ show ((Axis d1 s1, Axis d2 s2),(Axis d3 s3, Axis d4 s4))
  where
    same (Plus,Plus)   = NoTurn
    same (Minus,Minus) = OneEighty
    same (s1,s2) = error $ show (s1,s2) -- This would be a mirror reflection, which is not possible

    -- If we swap X&Y, 
--    swapped p1 p2 = toTurn' p1 - toTurn' p2
    swapped (Plus,Plus) (Plus,Plus)    = Clock          -- L
--    swapped (Plus,Plus) (Plus,Minus)   = CounterClock
    swapped (Plus,Plus) (Minus,Plus)   = Clock          -- B
    swapped (Plus,Plus) (Minus,Minus)  = Clock
--    swapped (Plus,Minus) (Plus,Plus)   = Clock
--    swapped (Plus,Minus) (Minus,Minus) = CounterClock
    swapped (Plus,Minus) (Minus,Plus)  = OneEighty
    swapped (Minus,Plus) (Plus,Plus)   = CounterClock
    swapped (Minus,Plus) (Minus,Plus)  = NoTurn
--    swapped (Minus,Plus) (Minus,Minus) = Clock
    swapped _ _ = error $ "BAD: " ++ show ((Axis d1 s1, Axis d2 s2),(Axis d3 s3, Axis d4 s4))
    
toTurn' (Plus,Plus)   = NoTurn
toTurn' (Plus,Minus)  = Clock
toTurn' (Minus,Minus) = OneEighty
toTurn' (Minus,Plus)  = CounterClock
-}
-------------------
{-

-- A puzzle is cube, a function from side and 2D tile coord to value

--project :: Cube' a -> (Factor (...) (V2 ) -> a)
--project 

--   (Axis D3 -> V2 Abs -> a) -> (Factor (...) (V2 Abs)

rotateCube :: Axis D3 -> Puzzle a -> Puzzle a
rotateCube ax1@(Axis a1 d1) f ax2@(Axis a2 d2) v@(V2 v1 v2) = f ax2 v
{-
        | a1 == a2 && d1 == d2 = f ax2 (rotateBy CounterClock v)    -- double -ve, because we look outside in
        | a1 == a2 && d1 /= d2 = f ax2 (rotateBy CounterClock v)
        | otherwise = f ax v'
  where
          ax = cross ax1 ax2        
          v' = v        -- 4 possibles, based on rotation of v
-}


-- This is about layout on screen only.
facePlacement'' :: Puzzle (Int,Int)
facePlacement'' ax (V2 a b) = case ax of
        (Axis X Plus)  -> (f (N.negate a), f (N.negate b))
        (Axis X Minus) -> (f a, f (N.negate b))
        (Axis Y Plus)  -> (f a, f b)
        (Axis Y Minus) -> (f a, f (N.negate b))
        (Axis Z Plus)  -> (f a, f (N.negate b))
        (Axis Z Minus) -> (f (N.negate a), f (N.negate b))
        (Axis _ _)     -> (f a, f (N.negate b))
  where
        f MinusOne = 0
        f Zero     = 1
        f PlusOne  = 2

-}
{-
-- rotate a Mega around a view point
rotateLayer :: Axis D3 -> Mega -> Mega
rotateLayer (Axis X Plus)  (V3 x y z) = V3 x z (N.negate y)
rotateLayer (Axis X Minus) (V3 x y z) = V3 x (N.negate z) y
rotateLayer (Axis Y Plus)  (V3 x y z) = V3 z y (N.negate x)
rotateLayer (Axis Y Minus) (V3 x y z) = V3 (N.negate z) y x
rotateLayer (Axis Z Plus)  (V3 x y z) = V3 y (N.negate x) z
rotateLayer (Axis Z Minus) (V3 x y z) = V3 (N.negate y) x z

-}
