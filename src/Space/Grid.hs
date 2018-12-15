module Space.Grid where 

import Prelude 

import Linear

import Space.Index

{- This module provides functions for working with 3-dimensional grids. 
 -}

-- | The width of an equilateral 3D grid in cells.
newtype Resolution a = Resolution Int deriving Show

class SpatialIndex g => SpatialGrid g where 
    -- | returns the number of grid cells encoded by this index 
    resolution :: g a -> Resolution a
    -- | creates a finer grid containing the same points as the input grid 
    subdivide :: SpatialGrid h => g a -> h a  
    -- | inserts a point into the grid 
    insertPoint :: Floating a => g a -> V3 a -> g a

newtype GridIndex = GridIndex Int deriving (Eq, Ord, Show)

-- | Returns grid coordinates corresponding to the given point
pointIndex :: RealFrac a => Resolution a -> V3 a -> GridIndex 
pointIndex (Resolution n) (V3 x y z) 
    = GridIndex $ ((round x * n) + round y) * n + round z

-- | Returns the origin of the grid cell occupying the grid index `idx` 
--   in a grid with resolution `res`.
origin :: Floating a => Resolution a -- ^ `res` 
                          -> GridIndex    -- ^ `idx` 
                          -> V3 a         -- ^ `cell origin`
origin (Resolution n) (GridIndex i) 
    = let 
        w         = fromIntegral n 
        (i'  , z) = i   `quotRem` n 
        (i'' , y) = i'  `quotRem` n 
        (_   , x) = i'' `quotRem` n 
      in 
        V3 (fromIntegral x / w) 
           (fromIntegral y / w)
           (fromIntegral z / w)      

-- | Returns the centroid of the grid cell occupying the given 
-- index in a grid with the given resolution.
centroid :: Floating a => Resolution a -> GridIndex -> V3 a 
centroid res@(Resolution n)
    = let 
        off = 1 / ( 2 * fromIntegral n ) 
      in 
        (V3 off off off ^+^) . origin res