module Rubik.Test where
        
import Rubik.Shape
import Rubik.Face

clk :: Face Square
clk = id

main :: IO ()
--main = shape (tile "red" `overlay` text 5 "Hello")

drawFace :: Face Shape -> Shape
drawFace face = background "#202020" 0.01 0.1
                       $ drawMap facePlacement $ 
                           fmap (borderShape 0.05) $ face

face = \ k -> tile "red" `overlay` (text 10 $ show k)

--main = shape (drawFace $ face)

main = shape (drawFace $ face . clockwise . clockwise . clockwise)
