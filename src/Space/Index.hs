module Space.Index where

import Prelude 

import Data.Vector 

import Linear

-- | Containers which index points by locations in a three dimensional grid.
class SpatialIndex f where 
    -- | returns the set of points in the index
    points :: Floating a => f a -> Vector (V3 a)
