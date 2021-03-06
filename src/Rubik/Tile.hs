module Rubik.Tile where
        
import Rubik.Abs as A
import Rubik.Axis
import Rubik.Negate as N
import Rubik.Sign as S
import Rubik.Turn as T
import Rubik.V3
import Rubik.Key
import Rubik.D3
import Rubik.V2


data Layer = E Sign      -- -2 or 2
           | I Abs       -- -1 | 0 | 1
           deriving (Eq, Ord, Show)

instance Key Layer where
    universe = map E universe ++ map I universe

instance Negate Layer where
    negate (E s) = E (N.negate s)
    negate (I a) = I (N.negate a)

data Tile = Tile (Axis D3) (V2 Abs)

instance Key Tile where
    universe = [ Tile a b | a <- universe, b <- universe ]


rotateTitle :: Axis D3 -> Tile -> Tile
rotateTitle = error ""
