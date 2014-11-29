{-# LANGUAGE OverloadedStrings #-}
module Rubik.Shape where

import Graphics.Blank
import Data.Text(Text,pack)

import Rubik.Key

shape :: Shape -> IO ()
shape s = blankCanvas 3000 $ \ context -> send context $ drawShape context $ borderShape 0.1 s

-- A Drawing is a picture with a finite size.
data Shape = Shape (Double,Double) (Canvas ())

instance Show Shape where
  show (Shape xy _) = show xy

-- Draw map, aka picture in 2D of a space.
drawMap :: (Key k) => (k -> (Int,Int)) -> (k -> Shape) -> Shape
drawMap pos dm = packShapes
        [ [ case lookup (col,row) xs' of
              Just k -> dm k
              Nothing -> emptyShape
          | col <- [0..cols]
          ]
        | row <- [0..rows]
        ]
  where rows = maximum $ map snd $ map pos universe   -- max row
        cols = maximum $ map fst $ map pos universe   -- max col
        xs   = [ (k,pos k) | k <- universe ]
        xs'  = [ (y,x) | (x,y) <- xs ]

emptyShape :: Shape
emptyShape = Shape (0,0) (return ())

-- Draw a compound shape as a large picutre
drawShape :: DeviceContext -> Shape -> Canvas ()
drawShape cxt (Shape (w',h') picture) = do
        (w,h) <- return (width cxt, height cxt)
        let w1 = w / w'
        let h1 = h / h'
        let s = min w1 h1
        save()
        translate(w/2,h/2)              -- center of the canvas
        translate(-s*w'/2,-s*h'/2)
        scale (s,s)
--        translate(-w'/2,-h'/2)        -- and shift for size of thing
        picture
        restore()

-- put a sized border round a shape.
borderShape :: Double -> Shape -> Shape
borderShape b (Shape (h,w) picture) = Shape (h+2*b,w+2*b) $ do
        save()
        translate (b,b)
        picture
        restore()

-- Rubik's tile.
tile :: Text -> Shape
tile col = Shape (1,1) $ do
          save()
          fillStyle col
          roundedBox (1/10) 1 1
          restore()

triangle :: Shape
triangle = Shape (1,1) $ saveRestore $ do
        beginPath()
        moveTo (0.5,0.3)
        lineTo (0.3,0.7)
        lineTo (0.7,0.7)
        closePath()
        fillStyle "black"
        fill()

circle :: Shape
circle = Shape (1,1) $ saveRestore $ do
        beginPath()
        arc(0.5, 0.5, 0.3, 0, 2 * pi, False)
        fillStyle "black"
        fill()
        
background :: Text -> Double -> Double -> Shape -> Shape
background col b cor shape@(Shape (x,y) _) = Shape (x',y') $ do
        save()
        fillStyle col
        roundedBox cor x' y'
        restore()
        pic
  where Shape (x',y') pic = borderShape b shape

packShapes :: [[Shape]] -> Shape
packShapes shapes = Shape (cols * maxW,rows * maxH) $ do
        sequence_ [ sequence_ [ do save()
                                   translate (col * maxW, row * maxH)
                                   s
                                   restore() 
                              | (Shape (w,h) s,col) <- ss `zip` [0..]
                              ]
                  | (ss,row) <- shapes `zip` [0..]
                  ]
  where
    cols = fromIntegral $ maximum (map length shapes)
    rows = fromIntegral $ length shapes
    maxW = maximum [ w | Shape (w,_) _ <- concat shapes ]
    maxH = maximum [ h | Shape (_,h) _ <- concat shapes ]

------------------------------------------------------------------------------------------------------

roundedBox :: Double -> Double -> Double -> Canvas ()
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

-- overlay
overlay :: Shape -> Shape -> Shape
overlay (Shape (x,y) c) (Shape (x',y') c') = Shape (max x x', max y y') $ do
        -- does not center yet
        c
        c'

text :: Int -> String -> Shape
text d strs = Shape (1,1) $ do
    save()
    scale (1/20,1/20)
    font $ pack $ show d ++ "px Calibri"
    sequence [ fillText(pack str,fromIntegral d * 0.1,fromIntegral d * (i + 0.70))
             | (i,str) <- [0,1..] `zip` lines strs
             ]
    restore()
