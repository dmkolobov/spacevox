module Implicit.March where 

import Prelude

import Data.Bits
import Data.Word 
import Data.Vector

import Linear

newtype MarchingCube = MarchingCube Word8

cube :: Floating a => [V3 a]
cube = [ V3 0.0 0.0 0.0
       , V3 1.0 0.0 0.0
       , V3 0.0 0.0 1.0
       , V3 1.0 0.0 1.0
       , V3 0.0 1.0 0.0
       , V3 1.0 1.0 0.0
       , V3 0.0 1.0 1.0
       , V3 1.0 1.0 1.0 ]

instance Semigroup MarchingCube where 
    MarchingCube a <> MarchingCube b = MarchingCube (a .|. b)

instance Monoid MarchingCube where 
    mempty = MarchingCube 0

-- | Construct a translation matrix from a 3-component vector.
moveTo3 :: Floating a => V3 a -> M44 a 
moveTo3 (V3 x y z) = V4 (V4 1.0 0.0 0.0 x)
                        (V4 0.0 1.0 0.0 y)
                        (V4 0.0 0.0 1.0 z)
                        (V4 0.0 0.0 0.0 1)

