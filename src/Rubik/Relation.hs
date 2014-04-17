module Rubik.Relation where

data Relation a b = Relation { apply :: a -> b, coapply :: b -> a }

