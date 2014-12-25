{-# LANGUAGE OverloadedStrings #-}
module Rubik.Drawing where

import Graphics.Blank

import Rubik.Shape

import Rubik.Face -- for test
import Rubik.Cube -- for test
import Rubik.Color -- for test
import Graphics.Blank
import Control.Applicative
import Rubik.D3
import Rubik.Axis

import Data.Text(Text,pack)
{-
main :: IO ()
main = blankCanvas 3000 $ \ context -> do
        send context $ do
                drawShape context
                  $ borderShape 0.1
                  $ drawMap cubePlacement cube
 where
  cube :: Axis D3 -> Shape
  cube = fmap (borderShape 0.01)
       $ fmap face
       $ fmap (pack . showColor)
       $ start

  face :: Text -> Shape
  face col = background "#202020" 0.01 0.1
                       $ drawMap facePlacement $ 
                           fmap (borderShape 0.05) $ 
                           fmap tile $
                           pure col


facePlacement :: a -> (Int, Int)
facePlacement = undefined
                
--foreground :: String -> Float -> Float -> Shape -> Shape


----------------------------------------------------------------------------------------------------

--beside :: Shape -> Shape -> Shape
--beside g h = (x,y) -> 


-}