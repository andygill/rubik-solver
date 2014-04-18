module Rubik.Shape where

import Graphics.Blank

import Rubik.Key

-- A Drawing is a picture with a finite size.
data Shape = Shape (Float,Float) (Canvas ())

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
drawShape :: Shape -> Canvas ()
drawShape (Shape (w',h') picture) = do
        (w,h) <- size
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
borderShape :: Float -> Shape -> Shape
borderShape b (Shape (h,w) picture) = Shape (h+2*b,w+2*b) $ do
        save()
        translate (b,b)
        picture
        restore()

-- Rubik's tile.
tile :: String -> Shape
tile col = Shape (1,1) $ do
          save()
          fillStyle col
          roundedBox (1/10) 1 1
          restore()

background :: String -> Float -> Float -> Shape -> Shape
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
