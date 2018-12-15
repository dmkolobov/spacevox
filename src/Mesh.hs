{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Mesh where 

import Prelude 

import Data.Vector as Vector
import Data.List as List

import Graphics.GL 
import Linear

newtype MeshGeometry = MeshGeometry ( [V4 GLfloat] -- ^ vertices
                                    , [V3 GLfloat] -- ^ normals ( one per vertex )
                                    ) deriving Show

data Triangle a = Triangle !(V4 a) !(V4 a) !(V4 a)

{- default implementations ----------------------------------------------------
 ------------------------------------------------------------------------------
 -}

class Mesh m where 
    -- | Returns a container containing all vertices in triangle order, paired 
    -- with their surface normal.
    geometry :: m -> MeshGeometry

{- mesh indexing --------------------------------------------------------------
 ------------------------------------------------------------------------------
 -}

-- | Indexes collections of vertices
newtype VIndex = VIndex Int deriving (Eq, Ord)

-- | Indexes collections of faces
newtype FIndex = FIndex Int deriving (Eq, Ord)

class MeshIndex m where 
    -- | returns the set of faces adjacent to this vertex 
    vertexFaces :: m -> VIndex -> [FIndex]
    -- | returns the set of vertices comprising a face
    faceVertices :: m -> FIndex -> ( VIndex , VIndex , VIndex )

