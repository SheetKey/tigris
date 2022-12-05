module Tigris.OpenGL.Init where


-- opengl
import qualified Graphics.Rendering.OpenGL as GL


-- init vao, vba, ebo, and shader program
-- also set some settings for rendering
initOpenGL :: IO (GL.VertexArrayObject, GL.BufferObject, GL.BufferObject, GL.Program)
initOpenGL = do
  ---------------------------------------------------------------------
  -- opengl settings
  ---------------------------------------------------------------------
  -- opengl blending
  GL.blend GL.$= GL.Enabled
  GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  
  -- opengl primitive restart
  GL.primitiveRestartIndex GL.$= Just 10000

  -- depth test
  GL.depthFunc GL.$= Just GL.Less

  -- msaa
  GL.multisample GL.$= GL.Enabled
  ---------------------------------------------------------------------
  ---------------------------------------------------------------------
    
  -- create the vao, vbo, and ebo
  (vao, vbo, ebo) <- bufferInit GL.bindVertexArrayObject GL.$= Just vao

  -- create the shader program
  program <- loadShaders [ (GL.VertexShader, "./shaders/default.vert")
                         , (GL.FragmentShader, "./shaders/default.frag")
                         ]
  -- set the current program to be the program
  GL.currentProgram GL.$= Just program

  return (vao, vbo, ebo, program)
