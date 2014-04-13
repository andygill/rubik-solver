module Rubik.Map where

import qualified Data.Map as M
import Control.Applicative
        
newtype Map k a = Map (M.Map k a)

-- internal function; use (!)
lookup :: Ord k => Map k a -> k -> a
lookup (Map m) k = m M.! k

fill :: Ord k => [k] -> Map k k
fill ks = Map (M.fromList [ (k,k) | k <- ks ])

class Key k where
  coord :: Map k k
  (!) :: Map k a -> k -> a

instance Eq a => Eq (Map k a) where
   Map m1 == Map m2 = M.elems m1 == M.elems m2

instance Ord a => Ord (Map k a) where
   compare (Map m1) (Map m2) = compare (M.elems m1) (M.elems m2)

instance Functor (Map k) where
    fmap f (Map m) = Map (fmap f m)

instance Key k => Applicative (Map k) where
    pure a = fmap (const a) coord
    m1 <*> m2 = fmap (\ ix -> (m1 ! ix) (m2 ! ix)) coord
