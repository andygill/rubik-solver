module Rubik.Test where
        
import Rubik.Shape
import Rubik.Face
import Rubik.Turn
import Rubik.D3
import Rubik.Cube
import Rubik.Color
import Rubik.Axis
import Rubik.Sign

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

face :: String -> Face Shape
face col k = tile col `overlay` (text 10 $ show k)

cube :: Cube (Face Shape)
cube = fmap face
     $ fmap showColor
     $ start

--main = shape (drawFace $ face)
--main = shape (drawFace $ rotateBy Clock $ face "red")

--main = shape (drawCube $ fmap drawFace cube)

main = shape $ packShapes 
             [ [ drawCube $ fmap drawFace cube ]
             , [ drawCube $ fmap drawFace $ z cube ]
             ]

-- notations
z :: Cube a -> Cube a
z = rotateD3 (Axis Z Plus) Clock