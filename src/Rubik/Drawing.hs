module Rubik.Drawing where

import Graphics.Blank

import Rubik.Map

import Rubik.Face -- for test
import Graphics.Blank

-- A Drawing is a picture with a finite size.
data Drawing = Drawing (Float,Float) (Canvas ())

type Shape = (Float,Float) -> Drawing

{-
drawMap :: Map k (Float,Float) -> Map k Drawing -> Drawing
drawMap pos subDrawings = do
    let poss = elems pos
    return poss

example :: Drawing
example = drawMap facePlacement (return (1,2))
-}

main = blankCanvas 3000 $ \ context -> do
        send context $ do
                drawShape $ scaleShape 0.9 $ \ (w,h) ->
                        tile "red" (w,h)
{-
                save()
                fillStyle "red"
                translate(100,100)
                roundedBox 10 100 100
                restore()


                moveTo(50,50)
                lineTo(200,300)
                lineWidth 10
                strokeStyle "red"
                stroke()
-}

drawShape :: Shape -> Canvas ()
drawShape f = do
        (w,h) <- size
        let Drawing (w',h') picture = f (w,h)
        save()
        translate(w/2,h/2)      -- center of the canvas
        translate(-w'/2,-h'/2)  -- and shift for size of thing
        picture
        restore()

scaleShape :: Float -> Shape -> Shape
scaleShape s f (x,y) = f (x*s,y*s)

tile :: String -> Shape
tile col (x,y) = Drawing (xy,xy) $ do
          save()
          fillStyle col
          roundedBox (xy/10) xy xy
          restore()
   where
        xy = min x y

----------------------------------------------------------------------------------------------------

--beside :: Shape -> Shape -> Shape
--beside g h = (x,y) -> 

--packShapes :: [[Shape]] -> Shape
--packShapes shapes

----------------------------------------------------------------------------------------------------

roundedBox :: Float -> Float -> Float -> Canvas ()
roundedBox radius width height = do
  save()
  beginPath()
  moveTo(radius, 0)
  lineTo(width - radius, 0)
  quadraticCurveTo(width, 0, width, radius)
  lineTo(width, height - radius)
  quadraticCurveTo(width, height, width - radius, height)
  lineTo(radius, height)
  quadraticCurveTo(0, height, 0, height - radius)
  lineTo(0, radius)
  quadraticCurveTo(0, 0, radius, 0)
  closePath()
  fill()
  restore()