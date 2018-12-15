{-# LANGUAGE MultiParamTypeClasses #-}

module Implicit.Voxel where 

import Prelude

import Data.Bits
import Data.Word

import Linear
import Graphics.GL

import System.IO.Unsafe

import Mesh
import Model

import Data.Vector hiding (foldl, zip, filter)

import Chunked

import Implicit.Voxel.Types

import Geometry

import Space.Index
import Space.Grid

newtype VoxelSpace a = VoxelSpace (ChunkSpace (Voxel8 a))

fillTransform :: Floating a => V3 a -> a -> M44 a 
fillTransform point size 
  = let 
      o = size * 0.25 
    in 
      moveTo (point ^+^ V3 o o o) !*! uniformScale (size * 0.5) 

testCube :: Floating a => V4 a -> a -> Vector (V4 a)
testCube (V4 x y z _) size = fmap (fillTransform (V3 x y z) size !*) 
                           . fmap point
                           $ points (Voxel8 255)

fillSpace :: Floating a => V4 a 
                        -> a 
                        -> Int 
                        -> (V4 a -> Bool)
                        -> ChunkSpace (Voxel8 a) 
fillSpace origin size n pred
  = chunkVolume origin size (Resolution n) testChunk
  where
    testVox i p = Voxel8 $ if pred p then bit i else 0
    testChunk o = foldMap id . imap testVox . testCube o

testVoxel :: (Ord a , Floating a) => Int -> ChunkSpace (Voxel8 a)
testVoxel n = fillSpace (V4 0.5 0.5 0.5 1.0) 1.0 n 
            $ \(V4 x y z _) -> sqrt ((x - 0.5)^2 + (y - 0.5)^2 + (z - 0.5)^2) < 0.5

v1 = [Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 5,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 0,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 5,Voxel8 5,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 0,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 5,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 5,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 1,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 1,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 5,Voxel8 0,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 1,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 5,Voxel8 5,Voxel8 1,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 3,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 0,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 5,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 1,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 1,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 0,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 1,Voxel8 0,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 1,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 7,Voxel8 5,Voxel8 1,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 0,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 0,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 3,Voxel8 0,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 1,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 1,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 3,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 1,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 7,Voxel8 3,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 7,Voxel8 3,Voxel8 1,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 3,Voxel8 3,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 7,Voxel8 7,Voxel8 3,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 3,Voxel8 3,Voxel8 1,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 1,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0,Voxel8 0]


-- -}