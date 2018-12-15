module GL where 

import Prelude

import Foreign.Ptr
import Graphics.UI.GLUT

-- | Wraps a BufferObject containing vertex data.  
data VertexBuffer = VertexBuffer { vbuffer :: !BufferObject 
                                 , size    :: !GLsizeiptr 
                                 , count   :: !NumArrayIndices 
                                 }

newtype CameraState = CameraState ( Float , Float ) 
