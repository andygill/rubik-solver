{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, FlexibleContexts #-}
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

import Graphics.Blank (blankCanvas, send, events,wait, eType, eWhich)
import qualified Data.Char as Char

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

main' = shape $ packShapes 
             [ [ drawCube $ cube ]
             , [ drawCube $ rotate (Turn3D t Z) $ cube | t <- universe ]
             ]


main = blankCanvas 3000 { events = ["keypress"] } $ \ context -> 
     let loop cube' = do
            send context $ drawShape context 
                         $ borderShape 0.1 
                         $ packShapes [ [ drawCube $ cube, drawCube $ cube' ] ]
            e <- wait context
            print e
            if eType e == "keypress" 
            then case eWhich e of
                   Just ch -> loop $ keypress (Char.chr ch) cube' cube
                   _ -> loop cube'
            else loop cube'
     in loop cube                             

-- keypress :: a -> a -> 
keypress :: Char -> Puzzle a -> Puzzle a -> Puzzle a
keypress 'x' = const . rotate (Turn3D Clock X) 
keypress 'y' = const . rotate (Turn3D Clock Y) 
keypress 'z' = const . rotate (Turn3D Clock Z) 
keypress 'X' = const . rotate (Turn3D CounterClock X) 
keypress 'Y' = const . rotate (Turn3D CounterClock Y) 
keypress 'Z' = const . rotate (Turn3D CounterClock Z) 
keypress ' ' = flip const  -- restore original
keypress _   = const       -- do nothing
        

---------------------------------------------------------------------------


-- twist ::
--twist :: Turn3D -> Puzzle a -> Puzzle a
--twist turn@(Turn3d t d) cube = 
        
side :: Turn3D -> Puzzle Bool
side turn@(Turn3D t d) = fmap (\ (V3 x y z) -> True) coord
  where coord :: Puzzle Mega
        coord = mega <$> puzzleSide <*> puzzleSquare
        

class Crown c where
  crown :: c -> Bool

instance Crown Sign where
  crown Plus = True
  crown Minus = False

instance (Negate c, Crown c) => Crown (Axis c) where
  crown (Axis c Plus)  = crown c
  crown (Axis c Minus) = crown (N.negate c)
          
instance Crown Abs where
  crown PlusOne = True
  crown _       = False 

instance Crown Layer where
  crown (E sgn) = crown sgn
  crown (I abs) = crown abs  



{-
crown :: Sign -> Layer -> Bool
crown sgn (E sgn') = sgn == sgn'
crown sgn (I abs') = 

data Layer = E Sign      -- -2 or 2
           | I Abs       -- -1 | 0 | 1
           deriving (Eq, Ord, Show)

Minus | Plus
-}