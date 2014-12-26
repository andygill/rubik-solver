module Rubik.Axis where

import Data.Ix
 
import Rubik.Negate     as N
import Rubik.Sign       as S
import Rubik.Key        as K

-- Perhaps call Axis, or Unit, or Signed.
data Axis dim = Axis dim Sign
    deriving (Eq,Ord,Ix)
    
instance Show dim => Show (Axis dim) where
    show (Axis dim sgn) = show dim ++ show sgn

instance Negate (Axis dim) where
    negate (Axis dim dir) = Axis dim (N.negate dir)

instance Key d => Key (Axis d) where
   universe = [ Axis d s | d <- universe, s <- universe ]

   