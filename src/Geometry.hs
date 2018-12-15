module Geometry where

import Prelude 

import Foreign.Ptr 
import Foreign.Storable
import Foreign.Marshal.Array

import Data.Vector

import Linear

-- | Returns a translation matrix
moveTo :: Floating a => V3 a -> M44 a 
moveTo (V3 x y z) = V4 (V4 1 0 0 x)
                       (V4 0 1 0 y)
                       (V4 0 0 1 z)
                       (V4 0 0 0 1)

-- | Returns a scaling matrix.
uniformScale :: Floating a => a -> M44 a 
uniformScale r = V4 (V4 r 0 0 0) 
                    (V4 0 r 0 0) 
                    (V4 0 0 r 0) 
                    (V4 0 0 0 1)
