{-# LANGUAGE ScopedTypeVariables #-}

module Tigris.OpenGL.Init where


-- opengl
import qualified Graphics.Rendering.OpenGL as GL

-- mylib
import Tigris.OpenGL.Buffer
import Tigris.OpenGL.Shader
import Tigris.OpenGL.Matrix

-- sdl
import qualified SDL

-- linear
import Linear

-- base
import Data.Foldable (traverse_)
  

-- init vao, vba, ebo, and shader program
-- also set some settings for rendering
initOpenGL :: SDL.Window -> IO (GL.VertexArrayObject, GL.BufferObject, GL.BufferObject, GL.Program)
initOpenGL window = do
  ---------------------------------------------------------------------
  -- opengl settings
  ---------------------------------------------------------------------
  -- opengl blending
  GL.blend GL.$= GL.Enabled
  GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  
  -- depth test
  GL.depthFunc GL.$= Just GL.Less

  -- msaa
  GL.multisample GL.$= GL.Enabled

  -- clear color
  GL.clearColor GL.$= GL.Color4 0.4 0.8 1.0 1.0
  ---------------------------------------------------------------------
  ---------------------------------------------------------------------
    
  -- create the vao, vbo, and ebo
  (vao, vbo, ebo) <- bufferInit
  GL.bindVertexArrayObject GL.$= Just vao

  -- create the shader program
  program <- loadShaders [ (GL.VertexShader, "./shaders/default.vert")
                         , (GL.FragmentShader, "./shaders/default.frag")
                         ]
  -- set the current program to be the program
  GL.currentProgram GL.$= Just program

  -- sdl window size
  (SDL.V2 w h) <- SDL.get $ SDL.windowSize window

  -- set opengl viewport
  GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))

  -- set the initial projection matrix
  locProj <- GL.get . GL.uniformLocation program $ "proj"
  let pm :: M44 GL.GLfloat = projectionMatrix 45 800 600
  (toMatrix pm :: IO (GL.GLmatrix GL.GLfloat)) >>= (GL.uniform locProj GL.$=) 
  
  _ <- traverse_ (putStrLn . show) <$> (GL.get GL.errors)

  return (vao, vbo, ebo, program)
