module Tigris.OpenGL.Buffer where

-- base
import Foreign.Storable
import Foreign.Ptr

-- vector
import qualified Data.Vector.Storable as V

-- opengl
import qualified Graphics.Rendering.OpenGL as GL


bufferDataWithVector :: (V.Storable a) => V.Vector a -> GL.BufferTarget -> GL.BufferUsage -> IO ()
bufferDataWithVector v target usage =
  V.unsafeWith v $ \ptr ->
                     GL.bufferData target GL.$=
                     ( fromIntegral $ V.length v * sizeOf (V.head v)
                     , ptr
                     , usage
                     )
bufferDataWithVectorEBO :: (V.Storable a) => V.Vector a -> GL.BufferTarget -> GL.BufferUsage -> IO ()
bufferDataWithVectorEBO v target usage =
  V.unsafeWith v $ \ptr ->
                     GL.bufferData target GL.$=
                     ( fromIntegral $ V.length v * V.length v
                     , castPtr ptr
                     , usage
                     )

bufferInit :: IO (GL.VertexArrayObject, GL.BufferObject, GL.BufferObject)
bufferInit = do
  vao <- (GL.genObjectName :: IO GL.VertexArrayObject)
  GL.bindVertexArrayObject GL.$= Just vao

  vbo <- (GL.genObjectName :: IO GL.BufferObject)
  GL.bindBuffer GL.ArrayBuffer GL.$= Just vbo

  ebo <- (GL.genObjectName :: IO GL.BufferObject)
  GL.bindBuffer GL.ElementArrayBuffer GL.$= Just ebo

  -- location for coordinate
  GL.vertexAttribPointer (GL.AttribLocation 0) GL.$=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor
      3
      GL.Float
      (fromIntegral $ 5 * sizeOf (undefined :: GL.GLfloat))
      (intPtrToPtr 0)
    )
  GL.vertexAttribArray (GL.AttribLocation 0) GL.$= GL.Enabled
  -- location for texture position
  GL.vertexAttribPointer (GL.AttribLocation 1) GL.$=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor
      2
      GL.Float
      (fromIntegral $ 5 * sizeOf (undefined :: GL.GLfloat))
      (intPtrToPtr (fromIntegral $ 3 * sizeOf (undefined :: GL.GLfloat)))
    )
  GL.vertexAttribArray (GL.AttribLocation 1) GL.$= GL.Enabled

  GL.bindVertexArrayObject GL.$= Nothing
  GL.bindBuffer GL.ElementArrayBuffer GL.$= Nothing

  return (vao, vbo, ebo)
