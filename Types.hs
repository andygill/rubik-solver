{-# LANGUAGE FlexibleInstances #-}
-- standard notation for the sides
-- the front, clockwise round the others, the back
module Types where

import Data.Array
import Control.Applicative
import Data.Maybe
import Data.List
import Prelude hiding (Left,Right)

------------------------------------------------------------------------------

data File = Left | Center | Right
		deriving (Eq,Ord,Enum,Show,Ix)


data Rank = Top | Middle | Bottom
		deriving (Eq,Ord,Enum,Show,Ix)

corners = ((Top,Left),(Bottom,Right))

------------------------------------------------------------------------------

newtype Face a = Face (Array (Rank,File) a)	-- 3x3

instance Eq a => Eq (Face a) where
        f1 == f2 = elemsFace f1 == elemsFace f2

instance Ord a => Ord (Face a) where
        f1 `compare` f2 = elemsFace f1 `compare` elemsFace f2

instance (Show a) => Show (Face a) where
   show (Face arr) = unlines $
                   [ "+---+" ] ++
		   [ "|" ++ concat [ show (arr ! (rank,file)) | file <- [Left  .. Right] ] ++ "|"
		     | rank <- [Top .. Bottom ]
		   ] ++
                   [ "+---+" ]

instance Functor Face where
  fmap f (Face face) = Face (fmap f face)

instance Applicative Face where
  pure a = Face $ array corners [ (i,a) | i <- range corners ]
  (Face f) <*> (Face a) = Face $ array corners [ (i,(f ! i) (a ! i)) | i <- range corners ]

-- Not a monad

elemsFace :: Face a -> [[a]]
elemsFace (Face face) = [ [ face ! (a,b) | b <- [Left .. Right] ] |  a <- [Top .. Bottom]]


lookupFace :: Face a -> Rank -> File -> a
lookupFace (Face a) rank file = a ! (rank,file)


newFace :: [[a]] -> Face a
newFace xss =
        Face $ array corners
		   [  ((rank,file),x)
		   | (rank,xs) <- zip [Top .. Bottom ] xss
		   , (file,x) <- zip [Left  .. Right] xs
		   ]


------------------------------------------------------------------------------

data Side      = F | U | R | D | L | B
		deriving (Eq,Ord,Enum,Show,Ix)

sides = (F,B)

newtype Cube a = Cube (Array Side (Face a))
	deriving (Eq,Ord)

elemsCube :: Cube a -> [Face a]
elemsCube (Cube a) = elems a


newCube :: [Face a] -> Cube a
newCube faces = Cube $ array sides
		[ (x,face)
		| (x,face) <- zip (range sides) faces
		]

instance Functor Cube where
  fmap f (Cube arr) = Cube (fmap (fmap f) arr)


instance Applicative Cube where
  pure a = Cube $ array sides [ (i,pure a) | i <- range sides ]
  (Cube f) <*> (Cube a) = Cube $ array sides [ (i,(f ! i) <*> (a ! i)) | i <- range sides ]

instance Show a => Show (Cube a) where
  show (Cube faces) =
	u ++ "\n"
	          ++ unlines [ a ++ " " ++ b ++ " " ++ c ++ " " ++ d
			     | (a,b,c,d) <- zip4 (lines f) (lines r) (lines b) (lines l)
			     ] ++ "\n" ++
	d
     where
	f = show (faces ! F)
	u = show (faces ! U)
	r = show (faces ! R)
	d = show (faces ! D)
	l = show (faces ! L)
	b = show (faces ! B)

------------------------------------------------------------------------------

newtype X = X Char

instance Show X where
  show (X c) = [c]

------------------------------------------------------------------------------

lookupCube :: Cube a -> Side -> Face a
lookupCube (Cube sqs) file = sqs ! file

------------------------------------------------------------------------------

-- A way of showing a simple face
niceFace :: String -> Face X
niceFace str = newFace [[a,b,c],[d,e,f],[g,h,i]]
  where
          [a,b,c,d,e,f,g,h,i] = map X str

fFace = niceFace "11  1 111"
uFace = niceFace "2 22 2222"
rFace = niceFace "33 3  3  "
dFace = niceFace " 4  4  44"
lFace = niceFace "555  5  5"
bFace = niceFace "6  66 66 "

cube = newCube [fFace,uFace,rFace,dFace,lFace,bFace]

------------------------------------------------------------------------------

data Turn = Clock | OneEighty | CounterClock
     deriving (Eq,Ord,Show)

negateTurn :: Turn -> Turn
negateTurn Clock = CounterClock
negateTurn CounterClock = Clock
negateTurn OneEighty = OneEighty

class Rotate a where
        rotate :: Turn -> a -> a                -- pointwise rotation

instance Rotate (Face a) where
        rotate d (Face a) = Face $ array corners [ (rotate d ix,a) | (ix,a) <- assocs a]

instance Rotate (Rank,File) where
        -- Memoize!
        rotate Clock (r,f)
	       = ( toEnum (1 + f')
		 , toEnum (1 + negate r')
	         )
           where
	           r' = fromEnum r - 1
	           f' = fromEnum f - 1
        rotate OneEighty rf =  rotate Clock . rotate Clock $ rf
        rotate CounterClock rf =  rotate Clock . rotate Clock . rotate Clock $ rf

-- rotateCube :: Side -> Turn -> Cube Rotate
{-
rotateCube :: Side -> Turn -> Cube a -> Cube a  -- moves the sides
-- Symetry of axis
rotateCube D turn = rotateCube U (negateTurn turn)
rotateCube L turn = rotateCube R (negateTurn turn)
rotateCube B turn = rotateCube F (negateTurn turn)

-- Symetry of turns
rotateCube side Clock        = rotateCubeClock side
rotateCube side OneEighty    = rotateCube side Clock . rotateCube side Clock
rotateCube side CounterClock = rotateCube side Clock . rotateCube side Clock . rotateCube side Clock

rotateCubeClock :: Side -> Cube a -> Cube a  -- moves the sides
rotateCubeClock F = cube

-}

opposites :: [(Side,Side)]
opposites = [(F,B),(U,D),(R,L)] ++ map swap opposites

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

opposite :: Side -> Side
opposite = fromJust . flip lookup opposites

-- This is about projecting to a two-D plain
circ :: Side -> [Side]
circ F = [U,R,D,L]
circ U = [D,R,F,L]
circ R = [U,B,D,F]
circ o = flip (circ (opposite o))
  where flip (x:xs) = x : reverse xs


{-
------------------------------------------------------------------------------

-- cut a square from a board, from (-0.5,-0.5) to (0.5,0.5)
-- Outside the square is transparent
cutSquare :: Board RGB -> Board (Maybe RGB)
cutSquare sq = CB.zipWith (\ a b -> choose (CB.just a) CB.nothing b) sq ourMask
  where
        ourMask = CB.zipWith (\ a b -> choose b CB.false a) (scale 0.9 square) circle

cbSquare :: Float -> O RGB -> Board (Maybe RGB)
cbSquare rnd a =
                    (withMask a .$
                         ( scale 0.95
                         $ stack [ shape
                                 , others
                                 ]))
          where
                rnd' = (1 - rnd) / 2
                roundedSq = scale rnd $ circle
                shape = stack
                        [ move (rnd',rnd') roundedSq
                        , move (rnd',-rnd') roundedSq
                        , move (-rnd',rnd') roundedSq
                        , move (-rnd',-rnd') roundedSq
                        ]
                others = stack
                        [ scaleXY (1,1 - rnd) square
                        , scaleXY (1 - rnd,1) square
                        ]


cbFace :: O RGB -> Face (Board (Maybe RGB)) -> (Board (Maybe RGB))
cbFace bk face = squares `over` cbSquare 0.1 bk
 where
  squares =
        scale 0.31 $ stack $
        [ move (x,y) $ (arr ! (rank,file))
        | (rank,y) <- zip [Top,Middle,Bottom] [1,0,-1]
        , (file,x)  <- zip [Left,Center,Right] [-1,0,1]
        ]
  Face arr = face

cbCube :: O RGB -> Cube (Board (Maybe RGB)) -> (Board (Maybe RGB))
cbCube bk cube = stack
        [ move (x,y) $ scale 0.9 $ cbFace bk (arr ! face)
        | (face,x,y) <-
                [ (F,0,0)
                , (L,-1,0)
                , (R,1,0)
                , (U,0,1)
                , (D,0,-1)
                , (B,2,0)
                ]
        ]
  where
        Cube arr = cube

-}
