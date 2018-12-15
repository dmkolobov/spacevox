module Model where 

import Prelude hiding (head, foldl)

import Data.Ord

import Foreign.Ptr
import Graphics.GL (GLfloat, GLsizeiptr)

import Linear

import Data.List
import Data.Vector hiding (foldl, head, unzip, zip, replicate, unpack)

import Codec.Wavefront

import Mesh

{- | Utilities for manipulating 3D polygonal meshes.
 -}

-- some conversion functions ------------------------------------
-----------------------------------------------------------------

locationV4 :: Location -> V4 GLfloat
locationV4 (Location x y z w) = V4 x y z 1.0 

normalV3 :: Normal -> V3 GLfloat
normalV3 (Normal nx ny nz) = V3 nx ny nz

-- types for indexing into sequential collections of geometry parts

newtype NormIndex = NormIndex Int deriving (Eq, Ord)

newtype MeshTriangle = MeshTriangle ( VIndex -- point a
                                    , VIndex -- point b
                                    , VIndex -- point c
                                    )

newtype WithNormal a = WithNormal ( V3 GLfloat , MeshTriangle )

data ObjMesh = ObjMesh { vertices  :: Vector (V4 GLfloat)
                       , normals   :: Vector (V3 GLfloat)
                       , faces     :: Vector (WithNormal MeshTriangle)
                       }

lookupVertex :: ObjMesh -> VIndex -> V4 GLfloat 
lookupVertex mesh (VIndex vi) = vertices mesh ! (vi - 1)

lookupNormal :: WavefrontOBJ -> NormIndex -> Normal
lookupNormal obj (NormIndex ni) 
    = objNormals obj ! (ni - 1)

unpackFace :: WavefrontOBJ -> Face -> WithNormal MeshTriangle
unpackFace obj (Face ia ib ic _) = WithNormal
    ( maybe zero (normalV3 . lookupNormal obj . NormIndex) $ faceNorIndex ia
    , MeshTriangle ( VIndex $ faceLocIndex ia 
                   , VIndex $ faceLocIndex ib 
                   , VIndex $ faceLocIndex ic
                   )
    )

loadWavefront :: WavefrontOBJ -> ObjMesh 
loadWavefront obj 
    = ObjMesh { vertices = locationV4 <$> objLocations obj 
              , normals  = normalV3 <$> objNormals obj 
              , faces    = unpackFace obj . elValue <$> objFaces obj
              }   
            
instance Mesh ObjMesh where 
  geometry m = MeshGeometry 
             . unzip 
             . foldMap unpackFace
             . toList
             $ faces m 
    where 
      unpackFace (WithNormal ( norm , (MeshTriangle (ic, ia, ib))))
        = [ ( lookupVertex m ia , norm )
          , ( lookupVertex m ib , norm )
          , ( lookupVertex m ic , norm ) ]

wavefront :: FilePath -> IO MeshGeometry 
wavefront = fmap ( geometry 
                 . loadWavefront 
                 . either (error "Couldn't load mesh.") id )
          . fromFile