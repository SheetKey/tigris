{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where


-- tigris
import Tigris

-- opengl
import qualified Graphics.Rendering.OpenGL as GL

-- sdl2
import qualified SDL

-- apecs
import Apecs

-- vector
import qualified Data.Vector.Storable as V

-- linear
import Linear.V3
import Linear.Matrix

-- base
import Data.Foldable (traverse_)
import Foreign.Ptr
import Control.Concurrent

-- text
import Data.Text

main :: IO ()
main = initAndRun "Game Demo" gameLoop

vertices :: V.Vector GL.GLfloat
vertices = V.fromList
  [ -- positions     -- texture
     0.25,  0.51, 0.0,  1.0/5, 1.0
  ,  0.25,  0.01, 0.0,  1.0/5, 0.0
  , -0.25,  0.01, 0.0,  0.0, 0.0
  , -0.25,  0.51, 0.0,  0.0, 1.0

  ,  5.0 ,  0.0,  5.0,  1.0/5, 1.0
  ,  5.0 ,  0.0, -5.0,  1.0/5, 0.0
  , -5.0 ,  0.0, -5.0,  0.0, 0.0
  , -5.0,   0.0,  5.0,  0.0, 1.0
  ]

indices :: V.Vector GL.GLuint
indices = V.fromList
  [
  --  0, 1, 2, 3
  --, 10000
  --, 4, 5, 6, 7
    4, 5, 6, 7
  , 10000
  , 0, 1, 2, 3
  ]

gameLoop :: World -> SystemT' IO ()
gameLoop world = do
  liftIO $ do
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
    ---------------------------------------------------------------------
    ---------------------------------------------------------------------
    
    -- create the vao, vbo, and ebo
    (vao, vbo, ebo) <- bufferInit
    GL.bindVertexArrayObject GL.$= Just vao
    -- load the vertices into the vbo
    bufferDataWithVector vertices GL.ArrayBuffer GL.StaticDraw
    -- load the indices into the ebo
    bufferDataWithVector indices GL.ElementArrayBuffer GL.StaticDraw

    -- create the shader program
    program <- loadShaders [ (GL.VertexShader, "./shaders/default.vert")
                           , (GL.FragmentShader, "./shaders/default.frag")
                           ]
    -- set the current program to be the program
    GL.currentProgram GL.$= Just program
  
    -- -- load the texture
    Right texture0001 <- createTextureFromPNG "./sprites/Sprite-0001.png"

    -- print errors
    traverse_ (putStrLn . show) <$> (GL.get GL.errors)
  
    -- -- activate texture unit 0 and bind texture
    GL.activeTexture GL.$= GL.TextureUnit 0
    GL.textureBinding GL.Texture2D GL.$= Just texture0001
  
    -- -- get the shader uniform locations
    locTexture <- GL.get . GL.uniformLocation program $ "Texture"
    locView <- GL.get . GL.uniformLocation program $ "view"
    locProj <- GL.get . GL.uniformLocation program $ "proj"

    -- -- set the texture to the shader uniform
    GL.uniform locTexture GL.$= GL.TextureUnit 0
    -- -- set the view and proj uniform matrices
    let vm :: M44 GL.GLfloat = view (V3 0 0 0) (V3 0 2 5)
        pm :: M44 GL.GLfloat = projection 45 800 600
    (toMatrix vm :: IO (GL.GLmatrix GL.GLfloat)) >>= (GL.uniform locView GL.$=) 
    (toMatrix pm :: IO (GL.GLmatrix GL.GLfloat)) >>= (GL.uniform locProj GL.$=) 

    -- set opengl viewport
    GL.viewport GL.$= (GL.Position 0 0, GL.Size 800 600)
    
    -- set the open gl clear color
    GL.clearColor GL.$= GL.Color4 0.4 0.8 1.0 1.0

    -- clear the buffers
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    -- draw the square
    GL.drawElements GL.TriangleFan (fromIntegral $ V.length indices) GL.UnsignedInt nullPtr

  -- get the sdl window from apecs
  Window window <- get global

  -- sdl present the opengl window
  SDL.glSwapWindow window
  liftIO $ do
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    GL.drawElements GL.TriangleFan (fromIntegral $ V.length indices) GL.UnsignedInt nullPtr
  SDL.glSwapWindow window
  
  liftIO $ threadDelay 20000

  liftIO $ do
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    GL.drawElements GL.TriangleFan (fromIntegral $ V.length indices) GL.UnsignedInt nullPtr
  SDL.glSwapWindow window
  
  liftIO $ threadDelay 5000000
