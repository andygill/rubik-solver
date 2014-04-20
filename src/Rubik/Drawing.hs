module Rubik.Drawing where

import Graphics.Blank

import Rubik.Shape

import Rubik.Face -- for test
import Rubik.Cube -- for test
import Rubik.Color -- for test
import Graphics.Blank
import Control.Applicative

main :: IO ()
main = blankCanvas 3000 $ \ context -> do
        send context $ do
                drawShape 
                  $ borderShape 0.1
                  $ drawMap cubePlacement cube
 where
  cube = fmap (borderShape 0.01)
       $ fmap face
       $ fmap showColor
       $ start

  face col = background "#202020" 0.01 0.1
                       $ drawMap facePlacement $ 
                           fmap (borderShape 0.05) $ 
                           fmap tile $
                           pure col


                
--foreground :: String -> Float -> Float -> Shape -> Shape


----------------------------------------------------------------------------------------------------

--beside :: Shape -> Shape -> Shape
--beside g h = (x,y) -> 


