{-# LANGUAGE OverloadedStrings #-}
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
import Rubik.Abs

clk :: Face Square
clk = id

main :: IO ()
--main = shape (tile "red" `overlay` text 5 "Hello")

drawFace :: Face Shape -> Shape
drawFace face = background "#202020" 0.01 0.1
                       $ drawMap facePlacement $ 
                           fmap (borderShape 0.05) $ face

drawCube :: Cube Shape -> Shape
drawCube = borderShape 0.1
         . drawMap cubePlacement
         . fmap (borderShape 0.01)

face :: Text -> Face Shape
face col k = tile col `overlay` (text 10 $ show k)

cube :: Cube (Face Shape)
cube = fmap face
     $ fmap pack
     $ fmap showColor
     $  start

--main = shape (drawFace $ face)
--main = shape (drawFace $ rotateBy Clock $ face "red")

--main = shape (drawCube $ fmap drawFace cube)

main = shape $ packShapes 
             [ [ drawCube $ fmap drawFace $ cube ]
             , [ drawCube $ fmap drawFace $ z cube ]
             ]

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

-------------------


-- A cube is a function from side and 2D tile coord to value

type Face' a = V2 Abs -> a

type Cube' a = Axis D3 -> Face' a

