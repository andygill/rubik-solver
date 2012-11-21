{-# LANGUAGE FlexibleInstances, KindSignatures, GADTs #-}
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

data Face :: * -> * where
        Face :: Array (Rank,File) a -> Face a	-- 3x3
--        PermuteFace  :: ((Rank,File) -> (Rank,File)) -> Face (a -> a)

instance Eq a => Eq (Face a) where
        f1 == f2 = elemsFace f1 == elemsFace f2

instance Ord a => Ord (Face a) where
        f1 `compare` f2 = elemsFace f1 `compare` elemsFace f2

instance (Show a) => Show (Face a) where
   show f@(Face arr) = unlines $
                   bar ++
		   concat
	           [ [ "|" ++ concat [ show' (arr ! (rank,file)) ++ "|" | file <- [Left  .. Right] ] ] ++ bar
		     | rank <- [Top .. Bottom ]
		   ]
    where maxWidth = maximum $ map (length . show) $ concat $ elemsFace f
          show' n  = take maxWidth (show n ++ repeat ' ')
          bar      = [ "+" ++ concat [ take maxWidth (repeat '-') ++ "+" | _ <- [1..3]] ]
--   show (PermuteFace f) = show (pure f <*> coordFace)

instance Functor Face where
  fmap f (Face face) = Face (fmap f face)

instance Applicative Face where
  pure a = Face $ array corners [ (i,a) | i <- range corners ]
  (Face f) <*> (Face a) = Face $ array corners [ (i,(f ! i) (a ! i)) | i <- range corners ]
{-
  (PermuteFace f) <*> (Face a) = Face $ array corners [ (i,a ! (f i)) | i <- range corners ]
  -- F ((a -> a) -> (a -> a)) -> F (a -> a) -> F (a -> a)
  (Face a) <*> (PermuteFace f) = error "Ug 1"
  -- F ((a -> a) -> (a -> a)) -> F (a -> a) -> F (a -> a)
  (PermuteFace a) <*> (PermuteFace f) = error "Ug 2"
-}
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


coordFace :: Face (Rank,File)
coordFace = forAllFace id

permuteFace :: Face (Rank,File) -> Face a -> Face a
permuteFace (Face coord) (Face face)
        = Face $ array corners
                       [ (i,(face ! (coord ! i)))
                       | i <- range corners
                       ]


forAllFace :: ((Rank,File) -> a) -> Face a
forAllFace f = Face $ array corners [ (i,f i) | i <- range corners ]

--permuteFace :: Face (Rank,File) -> Face (a -> a)
--permuteFace (Face f) = PermuteFace (f !)
{-
permuteFace c = f <*>
        = Face $ array corners
                       [ (i,(face ! (coord ! i)))
                       | i <- range corners
                       ]
-}

f = newFace [[1,2,3],[4,5,6],[7,8,9]]

------------------------------------------------------------------------------

class Inverse a where
  inverse :: a -> a

------------------------------------------------------------------------------

data Turn = Clock | OneEighty | CounterClock
     deriving (Eq,Ord,Show)

instance Inverse Turn where
    inverse Clock = CounterClock
    inverse CounterClock = Clock
    inverse OneEighty = OneEighty

class Rotate a where
        rotate :: a -> a                -- rotate one turn clockwise
        rotate = rotateBy (Just Clock)

        rotateBy :: Maybe Turn -> a -> a      -- rotate by given
        rotateBy = maybe id (promoteClock rotate)

promoteClock :: (a -> a) -> Turn -> a -> a
promoteClock f Clock        = f
promoteClock f OneEighty    = f . f
promoteClock f CounterClock = f . f . f


rotate2 :: Turn -> (Rank,File) -> (Rank,File)
rotate2 = promoteClock $ \ (r,f) ->
                let
	           r' = fromEnum r - 1
	           f' = fromEnum f - 1
                in
	         ( toEnum (1 + f')
		 , toEnum (1 + negate r')
	         )

rotateFace :: Turn -> Face a -> Face a
rotateFace = permuteFace . forAllFace . rotate2 . inverse

instance Rotate a => Rotate (Face a) where
        rotateBy = maybe id (permuteFace . forAllFace . rotate2 . inverse)


---------------------------------------------------------------------------------

data Direction = MinusOne | One
        deriving (Eq,Ord,Enum,Ix)

instance Show Direction where
    show One = "+"
    show MinusOne = "-"

instance Inverse Direction where
    inverse One = MinusOne
    inverse MinusOne = One

data Dimension = X | Y | Z
        deriving (Eq,Ord,Show,Enum,Ix)

type UnitVector = (Dimension,Direction)

rotateVec :: Dimension -> UnitVector -> UnitVector
rotateVec d (vd,n) | d == vd = (d,n)
rotateVec X (Y,n) = (Z,inverse n)
rotateVec X (Z,n) = (Y,n)
rotateVec Y (X,n) = (Z,n)
rotateVec Y (Z,n) = (X,inverse n)
rotateVec Z (X,n) = (Y,inverse n)
rotateVec Z (Y,n) = (X,n)



---------------------------------------------------------------------------------
data Side      = F | U | R | D | L | B
		deriving (Eq,Ord,Enum,Show,Ix)

unitVectorToSide :: UnitVector -> Side
unitVectorToSide = undefined

sideToUnitVector :: Side -> UnitVector
sideToUnitVector = undefined

{-


sides = (F,B)

opposites :: [(Side,Side)]
opposites = [(F,B),(U,D),(R,L)] ++ map swap opposites

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

opposite :: Side -> Side
opposite = fromJust . flip lookup opposites

-- This is about projecting to a two-D plane, by listing neighbours, clockwise.
circ :: Side -> [Side]
circ F = [U,R,D,L]
circ U = [B,R,F,L]
circ R = [U,B,D,F]
circ o = flip (circ (opposite o))
  where flip (x:xs) = x : reverse xs

rotateSide :: Side -> Turn -> Side -> Side
rotateSide = promoteClock . rotateSideClock

rotateSideClock :: Side -> Side -> Side
rotateSideClock front side
        = case lookup side pairs of
            Nothing -> error "the impossible happens in rotateSideClock"
            Just side' -> side'
   where
        pairs     = zip neighbors (rotate neighbors)
                 ++ [ (s,s)
                    | s <- range sides
                    , s `notElem` neighbors
                    ]

        neighbors = circ front
        rotate xs = tail xs ++ [head xs]
-}
---------------------------------------------------------------------------------


---------------------------------------------------------------------------------

newtype Cube a = Cube (Array UnitVector a)
	deriving (Eq,Ord)

sides = ((X,MinusOne),(Z,One))

elemsCube :: Cube a -> [a]
elemsCube (Cube a) = elems a

newCube :: [a] -> Cube a
newCube faces = Cube $ array sides
		[ (x,face)
		| (x,face) <- zip (range sides) faces
		]

instance Functor Cube where
  fmap f (Cube arr) = Cube (fmap f arr)

instance Applicative Cube where
  pure a = Cube $ array sides [ (i,a) | i <- range sides ]
  (Cube f) <*> (Cube a) = Cube $ array sides [ (i,(f ! i) (a ! i)) | i <- range sides ]

instance Show a => Show (Cube a) where
  show (Cube faces) =
	u ++ "\n"
	          ++ unlines [ a ++ " " ++ b ++ " " ++ c ++ " " ++ d
			     | (a,b,c,d) <- zip4 (lines f) (lines r) (lines b) (lines l)
			     ] ++
	d
     where
	f = show (faces ! (Z,MinusOne))
	u = show (faces ! (Y,One))
	r = show (faces ! (X,One))
	d = show (faces ! (Y,MinusOne))
	l = show (faces ! (X,MinusOne))
	b = show (faces ! (Z,One))


permuteCube :: Cube UnitVector -> Cube a -> Cube a
permuteCube (Cube coord) (Cube cube)
        = Cube $ array sides
                       [ (i,(cube ! (coord ! i)))
                       | i <- range sides
                       ]

forAllCube :: (UnitVector -> a) -> Cube a
forAllCube f = Cube $ array sides [ (i,f i) | i <- range sides ]

coordCube :: Cube UnitVector
coordCube = forAllCube id

-- how the squares are layed out
projectCube :: Cube UnitVector
projectCube = forAllCube $ \ (dim,dir) -> case (dim,dir) of
        (X,_)        -> (Y,One)
        (Y,MinusOne) -> (Z,MinusOne)
        (Y,One)      -> (Z,One)
        (Z,_)        -> (Y,One)

------------------------------------------------------------------------------

data Puzzle a = Puzzle (Cube (Face a))

instance Show a => Show (Puzzle a) where
        show (Puzzle p) = show p

instance Functor Puzzle where
  fmap f (Puzzle arr) = Puzzle (fmap (fmap f) arr)
{-
instance Applicative Cube where
  pure a = Cube $ array sides [ (i,a) | i <- range sides ]
  (Cube f) <*> (Cube a) = Cube $ array sides [ (i,(f ! i) (a ! i)) | i <- range sides ]
-}

--mkPuzzle :: [Cube a] -> Puzzle a
--mkPuzzle

arrow n = niceFace ['.',c,'.',c,c,c,c,'.',c]
  where (c:_) = show n

mkPuzzle = Puzzle . newCube

puzzle = mkPuzzle (map arrow [1..6])

rotateCube :: Dimension -> Turn -> Cube a -> Cube a
rotateCube dim = permuteCube . forAllCube . promoteClock (rotateVec dim) . inverse

rotateCube' :: (Rotate a) => Dimension -> Turn -> Cube a -> Cube a
rotateCube' d t cube =
        pure rotateBy
         <*> rot
         <*> rotateCube d t cube
  where
      f :: Face C -> ()
      f = undefined
      rot = pure rotAmount
                  <*> coordCube
                  <*> projectCube
                  <*> (fmap (promoteClock (rotateVec d) t) $ rotateCube d t projectCube)

      -- from Minus Side
      dir X Z Y  = True
      dir X Y Z  = False
      dir Y Z X  = True
      dir Z X Z  = False
      dir Z Y X = True
      dir Z X Y  = False

      dir2 (d,MinusOne) a b = dir d a b
      dir2 (d,One)      a b = not (dir d a b)

      rotAmount _ v1 v2           | v1 == v2 = Nothing
      rotAmount _ (vd1,_) (vd2,_) | vd1 == vd2 = Just OneEighty
      rotAmount (d,n) (vd1,n1) (vd2,n2)
                | dir2 (d,n) vd1 vd2 /= (n1 /= n2)
                                            = Just Clock
                | otherwise                 = Just CounterClock


rotatePuzzle d t (Puzzle cube) =
        pure (maybe id rotateFace)
         <*> rot
         <*> rotateCube d t cube
  where
      f :: Face C -> ()
      f = undefined
      rot = pure rotAmount
                  <*> coordCube
                  <*> projectCube
                  <*> (fmap (promoteClock (rotateVec d) t) $ rotateCube d t projectCube)

      -- from Minus Side
      dir X Z Y  = True
      dir X Y Z  = False
      dir Y Z X  = True
      dir Z X Z  = False
      dir Z Y X = True
      dir Z X Y  = False

      dir2 (d,MinusOne) a b = dir d a b
      dir2 (d,One)      a b = not (dir d a b)

      rotAmount _ v1 v2           | v1 == v2 = Nothing
      rotAmount _ (vd1,_) (vd2,_) | vd1 == vd2 = Just OneEighty
      rotAmount (d,n) (vd1,n1) (vd2,n2)
                | dir2 (d,n) vd1 vd2 /= (n1 /= n2)
                                            = Just Clock
                | otherwise                 = Just CounterClock


--        $ pure (,) <*> projectCube
--                   <*> (rotateCube d t (pure (,) <*> cube <*> projectCube))

--s        cube

--        pure (,) <*> cube <*> facedirections
--        rotateCube




{-
rotateCubeSurfaces :: Side -> Turn -> Cube (Maybe Turn)


rotateSideWith :: Side -> Turn -> Side -> Maybe Turn
rotateSideWith front turn side =
  where
        -- where is my top
        top = head (circ side)
-}
------------------------------------------------------------------------------

--rotate :: (XYZ,Turn) -> Side -> Side

--instance Rotate Side where
--        -- Memoize!
--        rotate Clock (r,f)

--rotateCube :: Side -> Cube a -> Cube a

--rotateCube F

-- data Side      = F | U | R | D | L | B


------------------------------------------------------------------------------

newtype C = C Char

instance Show C where
  show (C c) = [c]

instance Rotate C where
   rotate = id

------------------------------------------------------------------------------

lookupCube :: Cube a -> UnitVector -> a
lookupCube (Cube sqs) file = sqs ! file

------------------------------------------------------------------------------

-- A way of showing a simple face
niceFace :: String -> Face C
niceFace str = newFace [[a,b,c],[d,e,f],[g,h,i]]
  where
          [a,b,c,d,e,f,g,h,i] = map C str

fFace = niceFace "11  1 111"
uFace = niceFace "2 22 2222"
rFace = niceFace "33 3  3  "
dFace = niceFace " 4  4  44"
lFace = niceFace "555  5  5"
bFace = niceFace "6  66 66 "

--cube = newCube [fFace,uFace,rFace,dFace,lFace,bFace]

data Color = Red | Orange | Blue | Green | White | Yellow
        deriving (Eq,Ord,Show)

cube = newCube [Red,Orange,Blue,Green,White,Yellow]

------------------------------------------------------------------------------
{-
class Permute f where
        Id
        permute :: (Idx x -> Idx ) -> f (a -> a)
-}


{-
-- rotateCube :: Side -> Turn -> Cube Rotate
rotateCube :: Side -> Turn -> Cube (a -> a)  -- moves the sides
-- Symetry of axis
rotateCube D turn = rotateCube U (inverse turn)
rotateCube L turn = rotateCube R (inverse turn)
rotateCube B turn = rotateCube F (inverse turn)

-- Symetry of turns
rotateCube side Clock        = rotateCubeClock side
--rotateCube side OneEighty    = rotateCube side Clock . rotateCube side Clock
--rotateCube side CounterClock = rotateCube side Clock . rotateCube side Clock . rotateCube side Clock

-- rotating U,R or F, by Clock.
rotateCubeClock :: Side -> Cube (a -> a)  -- moves the sides
rotateCubeClock F = undefined
{-
          Cube $ array (F,B) [ (s,case t of
			    Nothing   -> face
			    Just turn -> rotateFace turn face)
		     | (s',(s,t)) <- zip [U,F,R,B,L,D] thisTurn
		     , let face = faces ! s'
		     ]
  where
	Just thisTurn = lookup side turns
turns =
  [(U,[(U,Just Clock),(R,Nothing),(B,Nothing),(L,Nothing),(F,Nothing),(D,Just CounterClock)])
  ,(F,[(R,Just Clock),(F,Just Clock),(D,Just Clock),(B,Just CounterClock),(U,Just Clock),(L,Just Clock)])
  ,(R,[(B,Just OneEighty),(U,Nothing),(R,Just Clock),(D,Just OneEighty),(L,Just CounterClock),(F,Nothing)])
  ]
-}


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
-}