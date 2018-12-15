module Cube where 

import Prelude 

import Foreign.Ptr 
import Foreign.Storable 
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array 
import Foreign.Marshal.Utils

import Graphics.UI.GLUT hiding (lookAt, ortho, RowMajor)
import Graphics.GL hiding (GLdouble, GLsizeiptr, GLfloat, GLsizei, GLushort)

import Data.ObjectName 

import LoadShaders
import Data.IORef

import Model
import Linear
import Space

import GL

import Codec.Wavefront.IO

import Mesh
import Geometry 
import Chunked
import Implicit.Voxel

import Control.Monad

import Data.List

import Geometry as Geometry

import Space.Index 
import Space.Grid

import Lin.Storage
import Data.Vector hiding (tail, head, replicate, filter, length, zip, forM_, (++))

-- all of the points in a cube
positions :: [GLfloat]
positions = [ (-1.0), (-1.0), (-1.0), 1.0 
            , (-1.0), (-1.0), 1.0   , 1.0
            , (-1.0), 1.0   , (-1.0), 1.0 
            , (-1.0), 1.0   , 1.0   , 1.0 
            , 1.0   , (-1.0), (-1.0), 1.0 
            , 1.0   , (-1.0), 1.0   , 1.0 
            , 1.0   , 1.0   , (-1.0), 1.0 
            , 1.0   , 1.0   , 1.0   , 1.0
            ]       

-- the colors associated with each vertex of the cube
colors :: [GLfloat]
colors = [ 0.5, 0.5, 0.5, 0.5
         , 0.5, 0.5, 0.0, 0.5
         , 0.5, 0.0, 0.5, 0.5
         , 0.5, 0.0, 0.0, 0.5
         , 0.0, 0.5, 0.5, 0.5
         , 0.0, 0.5, 0.0, 0.5
         , 0.0, 0.0, 0.5, 0.5
         , 0.5, 0.5, 0.5, 0.5
         ]

strip1 :: [GLushort]
strip1 = [ 0, 1, 2, 3, 6, 7, 4, 5]

strip2 :: [GLushort]
strip2 = [2, 6, 0, 4, 1, 5, 3, 7]

cubeElements :: [GLushort]
cubeElements = strip1 ++ [0xFFFF] ++ strip2

data CubeDescriptor = CubeDescriptor { vao          :: !VertexArrayObject
                                     , program      :: !Program 
                                     , vertexBuffer :: !VertexBuffer
                                     , camera       :: !(IORef CameraState)
                                     , objCount     :: !GLsizei
                                     , voxBuffer    :: !BufferObject
                                     }

sizeM44F = 16 * sizeOf (0 :: GLfloat)      
sizeV4F  = 4 * sizeOf (0 :: GLfloat)                               

withTransforms :: (Storable a, Floating a) => Vector (M44 a) -> (Ptr a -> IO b) -> IO b 
withTransforms coll k
  = (storeTransformsByRow $ (RowMajor . Linear.transpose) <$> coll) >>= (k . castPtr)

initCube :: IO CubeDescriptor
initCube 
  = do 
      vao    <- genObjectName
      voxels <- genObjectName :: IO BufferObject
      print "init"
      let 
        position = AttribLocation 0 
        normal   = AttribLocation 1
        vox      = AttribLocation 3

        ChunkSpace res vxs = testVoxel 16
        txs = chunkTranslations (ChunkSpace res vxs) :: Vector (M44 GLfloat)
        
      print "chunks:"
      print $ length vxs
      print "voxels:" 
      print $ length txs
      print "chunk_vector length:"
      print (length . chunkVector $ Resolution 2)

      MeshGeometry ( vs , ns ) <- wavefront "cube.obj"

      program <- loadShaders [ ShaderInfo VertexShader   (FileSource "cube.vert")
                             , ShaderInfo FragmentShader (FileSource "cube.frag")]

      bindVertexArrayObject $= Just vao 
      currentProgram        $= Just program
      
      vertexBuffer <- withModel position normal vs ns return

      withTransforms txs $ \voxArray -> 
        do 


          bindBuffer ArrayBuffer $= Just voxels 

          bufferData ArrayBuffer $= ( fromIntegral $ sizeM44F * length txs
                                    , voxArray 
                                    , StaticDraw )  
                                    
          vertexAttribPointer (AttribLocation 2) $= ( ToFloat 
                                                    , VertexArrayDescriptor 4 Float (fromIntegral sizeM44F) 
                                                    $ nullPtr )
          vertexAttribArray (AttribLocation 2) $= Enabled 
          glVertexAttribDivisor 2 1

          vertexAttribPointer (AttribLocation 3) $= ( ToFloat 
                                                    , VertexArrayDescriptor 4 Float (fromIntegral sizeM44F)
                                                    $ plusPtr nullPtr (fromIntegral $ 1 * sizeV4F) )
          vertexAttribArray (AttribLocation 3) $= Enabled 
          glVertexAttribDivisor 3 1

          vertexAttribPointer (AttribLocation 4) $= ( ToFloat 
                                                    , VertexArrayDescriptor 4 Float (fromIntegral sizeM44F) 
                                                    $ plusPtr nullPtr (fromIntegral $ 2 * sizeV4F) )
          vertexAttribArray (AttribLocation 4) $= Enabled
          glVertexAttribDivisor 4 1

          vertexAttribPointer (AttribLocation 5) $= ( ToFloat 
                                                    , VertexArrayDescriptor 4 Float (fromIntegral sizeM44F) 
                                                    $ plusPtr nullPtr (fromIntegral $ 3 * sizeV4F) )
          vertexAttribArray (AttribLocation 5) $= Enabled
          glVertexAttribDivisor 5 1

          camera <- newIORef $ CameraState (20.0, 50.0)
          
          depthFunc $= Just Less
          cullFace $= Just Back 

          pure $ CubeDescriptor { vao          = vao
                                , program      = program 
                                , vertexBuffer = vertexBuffer
                                , camera       = camera
                                , objCount     = fromIntegral $ length txs
                                , voxBuffer    = voxels
                                }

initialView :: M44 Float
initialView = lookAt (V3 2.0 2.0 2.0) (V3 0.0 0.0 0.0) (V3 0.0 (1.0) 0.0)

cameraRotationX :: (Epsilon a, Floating a) => a -> M44 a 
cameraRotationX a = m33_to_m44 $ fromQuaternion $ axisAngle (V3 1 0 0) (pi * a / 180.0)

cameraRotationY :: (Epsilon a, Floating a) => a -> M44 a 
cameraRotationY a = m33_to_m44 $ fromQuaternion $ axisAngle (V3 0 1 0) (pi * a / 180.0)

cameraTransform :: CameraState -> M44 Float
cameraTransform (CameraState ( xa, ya ))
  = cameraRotationX xa !*! cameraRotationY ya

newMatrixArray :: IO (Ptr GLfloat) 
newMatrixArray = newArray $ replicate 16 (0 :: GLfloat)

drawCube :: CubeDescriptor -> IO () 
drawCube cube = 
  let
    prog                   = program cube
    (VertexBuffer vb _ vc) = vertexBuffer cube
  in  
    do
      
      clear [ ColorBuffer , DepthBuffer ]

      bindVertexArrayObject $= Just (vao cube) 

      bindBuffer ArrayBuffer $= Just vb

      viewLoc <- uniformLocation prog "view_matrix"
      projLoc <- uniformLocation prog "projection_matrix"

      CameraState ( xa , ya ) <- readIORef $ camera cube

      let vm = Geometry.moveTo (V3 0 0 (-50))
           !*! cameraRotationX xa 
           !*! cameraRotationY ya 
           !*! uniformScale (1)
          pm = Linear.perspective (pi / 4) 1.0 5 3000 

      vmPtr <- newMatrixArray 
      pmPtr <- newMatrixArray   

      writeM44 vmPtr vm 
      writeM44 pmPtr pm

      uniformMatrix4fv viewLoc 1 False vmPtr
      uniformMatrix4fv projLoc 1 False pmPtr

      print (objCount cube)

      bindBuffer ArrayBuffer $= Just (voxBuffer cube)

      drawArraysInstanced Triangles 0 vc (objCount cube)

      flush

-- The OpenGL package offers no interface for glUniformMatrix*fv yet
uniformMatrix4fv :: UniformLocation -> GLsizei -> Bool -> Ptr GLfloat -> IO ()
uniformMatrix4fv location count =
   glUniformMatrix4fv (uniformLocationToGLint location) count . marshalGLboolean
   where marshalGLboolean x = fromIntegral $ case x of
            False -> GL_FALSE
            True -> GL_TRUE
         -- MEGA HACK because UniformLocation is abstract
uniformLocationToGLint = read . head . tail . words . show

mkKeyboard :: CubeDescriptor -> KeyboardMouseCallback 
mkKeyboard cube (Char c) Down _ _ 
  = do 
      modifyIORef' (camera cube) f
      postRedisplay Nothing
  where 
    f camera@(CameraState (xa, ya)) =
      case c of 
        'w' -> CameraState ((xa + 10.0), ya)  
        's' -> CameraState ((xa - 10.0), ya)  
        'a' -> CameraState (xa, (ya + 10.0))  
        'd' -> CameraState (xa, (ya - 10.0))
        _   -> camera
mkKeyboard _ _ _ _ _ = return ()       

cubeExample :: IO () 
cubeExample = do 
    (progName, _args) <- getArgsAndInitialize
    -- configure the type of window( use RGBA color space )
    initialDisplayMode $= [ RGBAMode ]
    -- set window size
    initialWindowSize $= Size 512 512
    -- use OpenGL 4.3
    initialContextVersion $= (4, 3)
    initialContextProfile $= [ CoreProfile ]
    -- create a window. Now we can use OpenGL functions
    _ <- createWindow progName
    -- dunno yet 
    descriptor <- initCube
    -- set a display callback which will be called by GLUT whenever 
    -- it thinks the contents of the window need to be updated.
    displayCallback $= drawCube descriptor

    keyboardMouseCallback $= Just (mkKeyboard descriptor)
    -- infinite loop that works with windowing and OS to process 
    -- user input. 
    mainLoop
  