{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Rubik.Face where

import Prelude hiding (Left, Right)

import Data.Array as A
import Control.Applicative
import Rubik.Map as M

data File = Left | Center | Right
    deriving (Eq,Ord,Enum,Show,Ix)


data Rank = Top | Middle | Bottom
    deriving (Eq,Ord,Enum,Show,Ix)

data Square = Square Rank File
    deriving (Eq,Ord,Ix)
                
instance Show Square where
    show (Square r f) = [ head (show r) , head (show f) ]

corners :: (Square,Square)
corners = (Square Top Left,Square Bottom Right)

squares :: [[Square]]
squares = [ [ Square a b | b <- [Left .. Right] ] |  a <- [Top .. Bottom]]

instance Key Square where
  coord = fill $ concat $ squares
  (!) = M.lookup 

type Face a = M.Map Square a

instance Show a => Show (M.Map Square a) where
   show f = unlines $
                   bar ++
		   concat
	           [ [ "|" ++ concat [ show' (f M.! (Square rank file)) ++ "|" | file <- [Left  .. Right] ] ] ++ bar
		     | rank <- [Top .. Bottom ]
		   ]
    where maxWidth = maximum $ map (length . show) $ map (f M.!) $ concat $ squares
          show' n  = take maxWidth (show n ++ repeat ' ')
          bar      = [ "+" ++ concat [ take maxWidth (repeat '-') ++ "+" | _ <- [1..3]] ]


