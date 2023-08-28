{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Tigris.Init where

-- mylib
import Tigris.ECS
import Tigris.SDL
import Tigris.OpenGL.Init

-- sdl
import qualified SDL

-- apecs
import Apecs
import Apecs.Stores

-- text
import Data.Text

  
initAndRun :: Text -> (World -> SystemT' IO ()) -> IO ()
initAndRun winName gameLoop = do
  SDL.initializeAll
  win <- SDL.createWindow winName windowConfig
  _ <- SDL.glCreateContext win
  glBuffers <- initOpenGL win
  treeBuffers <- initOpenGLTree
  world <- initWorld
  runWith world $ do
    setReadOnly global $ Window win
    setReadOnly global $ GLBuffers glBuffers
    gameLoop world

--initPosition :: CInt -> CInt -> CInt -> CInt -> Position
--initPosition x y w h = let rect = mkRect x y w h in Position $ V4 rect rect rect rect

--initTexture :: MonadIO m => String -> SystemT' m Texture
--initTexture path = do
--  Renderer ren <- get global
--  texture <- SDLI.loadTexture ren path
--  return $ Texture texture
