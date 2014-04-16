module Rubik.Drawing where

import Graphics.Blank

import Rubik.Map

import Rubik.Face -- for test
import Graphics.Blank


drawMap :: (Ord k) => Map k (Int,Int) -> Map k Shape -> Shape
drawMap pos dm = packShapes
        [ [ case lookup (col,row) xs' of
              Just k -> dm ! k
              Nothing -> empty
          | col <- [0..cols]
          ]
        | row <- [0..rows]
        ]
  where rows = maximum $ map fst $ elems pos   -- max row
        cols = maximum $ map snd $ elems pos    -- max col
        xs   = toList pos
        xs'  = [ (y,x) | (x,y) <- xs ]

{-
example :: Drawing
example = drawMap facePlacement (return (1,2))
-}

-- A Drawing is a picture with a finite size.
data Shape = Shape (Float,Float) (Canvas ())

empty = Shape (0,0) (return ())

main = blankCanvas 3000 $ \ context -> do
        send context $ do
                drawShape $ borderShape 0.1
                       $ background "#202020" 0.01 0.1
                       $ drawMap facePlacement $ 
                           fmap (borderShape 0.02) $ 
                           fmap tile $
                           mkMap (\ s -> "red")


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

borderShape :: Float -> Shape -> Shape
borderShape b (Shape (h,w) picture) = Shape (h+2*b,w+2*b) $ do
        save()
        translate (b,b)
        picture
        restore()

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
                

----------------------------------------------------------------------------------------------------

--beside :: Shape -> Shape -> Shape
--beside g h = (x,y) -> 

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
