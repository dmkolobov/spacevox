{-# LANGUAGE MultiParamTypeClasses #-}

module Chunked where
  
import Prelude hiding (replicate, zip, filter)

import Foreign.Ptr 
import Foreign.Storable

import Foreign.Marshal.Array

import Data.Word
import Data.Vector 

import Linear
import Geometry

import Space.Index 
import Space.Grid

-- | Reference to a cell in a 3D grid.
data Chunk = Chunk !Int !Int !Int deriving Show

asPoint :: Floating a => Chunk -> V3 a 
asPoint (Chunk x y z) = fmap fromIntegral $ V3 x y z

-- | Implemented by 3D grid cells. 
class Chunked a where 
  -- | Encode the smallest chunk.
  encodeAtom :: a -> Word8 
  -- | Decode the smallest chunk. 
  decodeAtom :: Word8 -> a
  -- | The resolution of the sub-space encoded by each chunk.
  atomicResolution :: Resolution a   
  -- | The storage requirements (in bytes) of the smallest chunk.                       
  atomicSize :: a -> Int 
  -- | Returns whether the chunk is empty 
  isEmpty :: a -> Bool

-- | Returns a vector of cell references at the given resolution.              
chunkVector :: Resolution c -> Vector Chunk
chunkVector (Resolution n) = do x <- generate n id 
                                y <- generate n id 
                                z <- generate n id 
                                return $ Chunk x y z

data ChunkSpace c = ChunkSpace { res :: Resolution c 
                               , chunks     :: Vector c 
                               } deriving Show                               

chunkWidth :: (Floating a, Chunked c) => a -> Resolution c -> a 
chunkWidth size (Resolution n) = size / fromIntegral n

chunkVolume :: ( Floating a 
               , Chunked c ) => V4 a             -- ^ origin 
                             -> a                -- ^ volume side length
                             -> Resolution c     -- ^ cells per side
                             -> (V4 a -> a -> c) -- ^ chunk generator
                             -> ChunkSpace c
chunkVolume (V4 x y z _) size res f 
  = let 
      cw = chunkWidth size res
    in 
      ChunkSpace res . flip fmap (chunkVector res) $ \(Chunk cx cy cz) -> 
        f (V4 (fromIntegral cx * cw) 
              (fromIntegral cy * cw) 
              (fromIntegral cz * cw)
              1.0) 
          cw

chunkIndex :: Resolution c -> Chunk -> Int 
chunkIndex (Resolution n) (Chunk x y z) = ((x * n) + y) * n + z

indexChunk :: Resolution c -> Int -> Chunk 
indexChunk (Resolution n) i = let 
                                (i'  , z) = i   `quotRem` n 
                                (i'' , y) = i'  `quotRem` n 
                                (_   , x) = i'' `quotRem` n 
                              in 
                                Chunk x y z

chunkTranslations :: ( Floating a , SpatialIndex c ) => ChunkSpace (c a) -> Vector (M44 a)
chunkTranslations (ChunkSpace res@(Resolution n) chunks) 
  = let
      chunkOrigins = fmap asPoint $ chunkVector res
    in 
      foldMap f $ zip chunks chunkOrigins
  where 
    f ( c, p ) = let 
                   scale = 1 / 2
                 in 
                   fmap (\p' -> moveTo ((p ^* (1 / scale)) ^+^ p') !*! uniformScale scale) $ points c

chunkScale :: ( Floating a , Chunked c ) => ChunkSpace c -> a -> M44 a
chunkScale space size = let 
                          Resolution n = res space 
                        in 
                          uniformScale $ size / fromIntegral n
                          
