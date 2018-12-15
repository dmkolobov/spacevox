module Space where 

import Prelude 

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array

import Data.ObjectName (genObjectName)

import Graphics.UI.GLUT 
import Linear

import GL

import Codec.Wavefront.IO (fromFile)

{- | Predefined axis vectors for creation of quaternions.
 -}

xAxis :: V3 Double 
xAxis = V3 1 0 0 

yAxis :: V3 Double 
yAxis = V3 0 1 0 

zAxis :: V3 Double
zAxis = V3 0 0 1

-- | Given a vector containing x, y, and z rotations, return 
-- a rotation matrix combining the component rotations.
rotationMat :: V3 Double -> M33 Double 
rotationMat (V3 ax ay az)
  = fromQuaternion (Quaternion ax xAxis) !*!
    fromQuaternion (Quaternion ay yAxis) !*!
    fromQuaternion (Quaternion az zAxis)

{- Writing data ----------------------------------------------------------
   -----------------------------------------------------------------------
   
   Utilities for writing linear algebra types to memory owned by OpenGL.
 -}

-- | Write a 4-component vector to the location at ptr.
writeV4 :: Storable a => Ptr a
                      -> V4 a 
                      -> IO ()
writeV4 ptr (V4 a1 a2 a3 a4) = pokeArray ptr [a1, a2, a3, a4]

-- | Write a 4x4 matrix to the location at ptr.
writeM44 :: Storable a => Ptr a
                       -> M44 a 
                       -> IO ()
writeM44 ptr = inner . transpose 
  where 
    inner (V4 c1@(V4 a _ _ _) c2 c3 c4) 
      = let offset = 4 * sizeOf a 
        in 
          do writeV4 (plusPtr ptr (fromIntegral (0 * offset))) c1 
             writeV4 (plusPtr ptr (fromIntegral (1 * offset))) c2 
             writeV4 (plusPtr ptr (fromIntegral (2 * offset))) c3 
             writeV4 (plusPtr ptr (fromIntegral (3 * offset))) c4

{- Model utilities -------------------------------------------------------
   -----------------------------------------------------------------------}


dummyFloat = 0.0 :: GLfloat

withModel :: AttribLocation 
          -> AttribLocation 
          -> [V4 GLfloat]
          -> [V3 GLfloat] 
          -> (VertexBuffer -> IO a) 
          -> IO a
withModel vLoc nLoc vs ns k
  = let 
      vSize = sizeOf dummyFloat * 4 * (length vs + 1)
      nSize = sizeOf dummyFloat * 3 * length ns 
    in 
      do 
        bufferName <- genObjectName 

        bindBuffer ArrayBuffer $= Just bufferName

        bufferData ArrayBuffer $= ( fromIntegral $ vSize + nSize, nullPtr, StaticDraw )

        withArray vs $ \vp -> 
          withArray ns $ \np -> 
            do 
              bufferSubData ArrayBuffer WriteToBuffer 0 (fromIntegral vSize) vp 
              bufferSubData ArrayBuffer WriteToBuffer (fromIntegral vSize) (fromIntegral nSize) np

              vertexAttribPointer vLoc $= ( ToFloat 
                                          , VertexArrayDescriptor 4 Float 0 nullPtr)

              vertexAttribPointer nLoc $= ( ToFloat 
                                          , VertexArrayDescriptor 3 Float 0 
                                          $ plusPtr nullPtr (fromIntegral vSize) )          
                                          
              vertexAttribArray vLoc $= Enabled 
              vertexAttribArray nLoc $= Enabled

              k $ VertexBuffer
                { vbuffer = bufferName
                , size    = fromIntegral $ vSize + nSize 
                , count   = fromIntegral $ length vs  
                }

