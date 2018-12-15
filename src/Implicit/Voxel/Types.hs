module Implicit.Voxel.Types where 

import Data.Bits    
import Data.Word

import Data.Vector  

import Chunked

import Space.Index
import Space.Grid

import Linear

cube :: Floating a => Vector (V3 a)
cube = do x <- fromList [ 0 , 1 ] 
          y <- fromList [ 0 , 1 ]
          z <- fromList [ 0 , 1 ]
          return $ V3 x y z

-- | Represents a 2x2 cube
newtype Voxel8 a = Voxel8 Word8 deriving (Show, Eq)

instance Chunked (Voxel8 a) where 
  encodeAtom (Voxel8 v) = v
  decodeAtom v          = Voxel8 v
  atomicResolution      = Resolution 2
  atomicSize            = const 1
  
  isEmpty (Voxel8 0)    = True 
  isEmpty (Voxel8 1)    = True
  isEmpty (Voxel8 _)    = False

instance Semigroup (Voxel8 a) where 
  (Voxel8 a) <> (Voxel8 b) = Voxel8 $ a .|. b 

instance Monoid (Voxel8 a) where 
  mempty = Voxel8 0

instance SpatialIndex Voxel8 where 
  points (Voxel8 v) = foldMap (maybe mempty singleton) $ imap testVoxel cube
    where
      testVoxel i p = if testBit v i then Just p else Nothing
                          

data VoxelGrid a = VoxelGrid { totalR :: Resolution a        -- | total grid resolution
                             , levelR :: Resolution a        -- | resolution at the level
                             , voxels :: Vector (Voxel8 a) } -- | the voxels in the grid

instance SpatialIndex VoxelGrid where 
    points (VoxelGrid _ _ voxels) = foldMap points voxels

{- }
instance SpatialGird (Voxel8 a) where 

  subdivide vox = foldl insertPoint 
                        (VoxelGrid { totalR :: Resolution 4 
                                   , levelR :: Resolution 2 
                                   , voxels :: mempty })
                        (points vox)

  insertPoint (Voxel8 vox8) point
    = let  
        V3 x y z = normalize point
      in 
-}