{-# LANGUAGE MultiParamTypeClasses #-}
module Rubik.Turn where
        

class Turn v o where
  -- apply a unit turn to o, when viewed using v
  turn :: v -> o -> o
                  