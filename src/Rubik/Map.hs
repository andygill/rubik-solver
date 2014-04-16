module Rubik.Map where

import qualified Data.Map as M
import Control.Applicative
        
newtype Map k a = Map (M.Map k a)

(!) :: Ord k => Map k a -> k -> a
(!) (Map m) k = m M.! k

mkMap :: Key k => (k -> a) -> Map k a
mkMap f = Map (M.fromList [ (k,f k) | k <- universe ])

coord :: Key k => Map k k
coord = mkMap id

class Ord k => Key k where
  universe :: [k]

instance Eq a => Eq (Map k a) where
   m1 == m2 = elems m1 == elems m2

instance Ord a => Ord (Map k a) where
   compare m1 m2 = compare (elems m1) (elems m2)

instance Functor (Map k) where
    fmap f (Map m) = Map (fmap f m)

instance Key k => Applicative (Map k) where
    pure a = mkMap (const a) 
    m1 <*> m2 = fmap (\ ix -> (m1 ! ix) (m2 ! ix)) coord

-- do not depend on any specific order of this output
elems :: Map k a -> [a] 
elems (Map m) = M.elems m

toList :: Map k a -> [(k,a)]
toList (Map m) = M.toList m