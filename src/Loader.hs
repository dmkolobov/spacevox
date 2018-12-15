module Loader where 

import Prelude hiding (length)

import Foreign.Ptr 
import Foreign.Storable 
import Foreign.Marshal.Array 
import Foreign.Marshal.Utils

import Graphics.UI.GLUT hiding (lookAt, ortho)
import Graphics.GL hiding (GLdouble, GLsizeiptr, GLfloat, GLsizei, GLushort)

import Data.ObjectName 

import LoadShaders
import Data.IORef



import Data.String hiding (length)
import Data.ByteString hiding (length)

import Data.Vector

import Codec.Wavefront
import Codec.Wavefront.IO

import Linear
import Model 

import Space
import GL

data State = State { vao         :: !VertexArrayObject
                   , ebo         :: !BufferObject
                   , pro         :: !Program 

                   , viewMatrix  :: !(Ptr GLfloat)
                   , projMatrix  :: !(Ptr GLfloat) 

                   , cameraState :: !(IORef CameraState)
                   }
