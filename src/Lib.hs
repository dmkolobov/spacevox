module Lib
    ( someFunc
    , example1
    ) where

import Prelude hiding (init)
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Graphics.UI.GLUT
import Data.ObjectName
import LoadShaders

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | specifies an offset into a buffer object.
bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices

-- vertices given in Normalized-Device Coordinates [-1,1]
t1 :: [Vertex2 GLfloat]
t1 = [ Vertex2 (-0.90) (-0.90)
     , Vertex2   0.85  (-0.90)
     , Vertex2 (-0.90)   0.5]

t2 :: [Vertex2 GLfloat]
t2 = [ Vertex2   0.90  (-0.85)
     , Vertex2   0.90    0.90 
     , Vertex2 (-0.85)   0.90 ]

{- Note that in all binding calls, we can re-use the allocated names in the 
   future to bind to the objects that were allocated at the first binding. 
   
   The act of binding to an allocated name is like selecting an item from 
   a collection, with an initializer that is called once. 
   
   In Haskell, the fact that `genObjectName` is monadic -}

init :: IO Descriptor
init = do
  -- Instruct OpenGL to allocate an object name which will be used to 
  -- refer to a vertex-array object. This name acts much like a pointer
  -- in C. The regions of memory these names refer to are currently 
  -- uninitialized.
  triangles <- genObjectName
  -- Bind the allocated name to a new vertex array object, and mark it 
  -- as _current_. This means that any operations relavant to the bound 
  -- object will affect its state from now on.
  bindVertexArrayObject $= Just triangles

  let vertices    = t1 ++ t2               -- create a list of vertices
      numVertices = length vertices        -- find the number of vertices
      vertexSize  = sizeOf (head vertices) -- find the size of each vertex

  {- Allocate a name for a buffer object, which will be used to store our 
     vertex data. A buffer object is a memory region that is allocated and
     managed by the OpenGL server.
     
     This buffer object is bound to and managed by the current vertex-array
     object. 
     
     At this point, a weak analogy might be that the VAO is the "variable", 
     while the buffer object is the underlying storage location.-}
  arrayBuffer <- genObjectName

  {- Bind the allocated buffer object name to a binding point. This act creates 
     the buffer object.
     
     The ArrayBuffer data constructor acts as a binding point referred to as the 
     _buffer binding target_. -}
  bindBuffer ArrayBuffer $= Just arrayBuffer

  -- Temporarily stores our list of vertices in an array and invokes our 
  -- continuation with a pointer to this array.
  withArray vertices $ \ptr -> do
    -- compute the size of the buffer
    let size = fromIntegral (numVertices * vertexSize)
    {- Allocate OpenGL server storage memory for vertex data, and copy the data from 
       the temporary array pointed to by `ptr`. This array is referred to as "client"
       memory in the red book. -}
    bufferData ArrayBuffer $= ( size       -- number of storage units to allocate( bytes )
                              , ptr        -- pointer to client memory where data is stored
                                           -- NOTE: We may pass null to initialize a big buffer, and 
                                           --       we can use bufferSubData to fill specific regions of 
                                           --       it.
                              , StaticDraw -- hint regarding read/write access patterns
                              )

  program <- loadShaders [ ShaderInfo VertexShader (FileSource "triangles.vert")
                         , ShaderInfo FragmentShader (FileSource "triangles.frag")]

  currentProgram $= Just program

  {- Initializes the shader variable `vPosition` -}
  let firstIndex = 0
      vPosition = AttribLocation 0 -- corresponds to the location specified in the vertex 
                                   -- shader `vPosition` variable declaration. Presumably, 
                                   -- another variable would require another `AttribLocation` 
                                   -- value.
  -- Plumb the vPosition variable location to a corresponding offset in the currently bound 
  -- buffer object. 
  vertexAttribPointer vPosition $=
    (ToFloat,
    -- Describes the structure to be imposed on the buffer object
     VertexArrayDescriptor 2     -- number of components of the variable
                           Float -- data type of each component
                           0     -- stride, or the byte offset between consequtive elements in 
                                 -- the array. Zero means that the array is tightly packed, or that 
                                 -- the shader a single input argument. 
                           -- computes an offset from the start of the buffer object 
                           -- for the first set of values in the array
                           (bufferOffset (firstIndex * vertexSize)))
  
  -- enable the vertex array.
  vertexAttribArray vPosition $= Enabled

  return $
    Descriptor triangles (fromIntegral firstIndex) (fromIntegral numVertices)

display :: Descriptor -> DisplayCallback
display (Descriptor triangles firstIndex numVertices) = do
  -- Set the color that we'll use to clear the frame buffer. Note that imperative
  -- OpenGL state manipulation functions are represented as StateVars in haskell. 
  clearColor $= Color4 1.0 1.0 1.0 1.0

  clear [ ColorBuffer ]
  -- Bind the VAO object name. The bound VAO is the one created in the `init` method, 
  -- referencing the buffer object and structure we specified.
  bindVertexArrayObject $= Just triangles
  -- Send vertex data to the OpenGL pipeline. 
  drawArrays Triangles firstIndex numVertices
  flush

example1 :: IO ()
example1 = do
  -- initialize glut library
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
  descriptor <- init
  -- set a display callback which will be called by GLUT whenever 
  -- it thinks the contents of the window need to be updated.
  displayCallback $= display descriptor
  -- infinite loop that works with windowing and OS to process 
  -- user input. 
  mainLoop


