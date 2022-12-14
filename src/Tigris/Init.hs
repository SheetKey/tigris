{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Tigris.Init where

-- mylib
import Tigris.Graphics
import Tigris.ECS

-- sdl
import qualified SDL
import qualified SDL.Image as SDLI
import qualified SDL.Font as SDLF

-- apecs
import Apecs
import Apecs.Stores

-- text
import Data.Text

-- base
import Control.Monad.IO.Class

initAndRun :: Text -> (World -> SystemT' IO ()) -> IO ()
initAndRun winName gameLoop = do
  --SDL.initialize [ SDL.InitVideo, SDL.InitEvents, SDL.InitGameController ]
  SDL.initializeAll
  SDLI.initialize [ SDLI.InitPNG ]
  SDLF.initialize
  win <- SDL.createWindow winName windowConfig
  ren <- SDL.createRenderer win (-1) SDL.defaultRenderer
  world <- initWorld
  runWith world $ do
    setReadOnly global $ Window win
    setReadOnly global $ Renderer ren
    gameLoop world

initPosition :: CInt -> CInt -> CInt -> CInt -> Position
initPosition x y w h = let rect = mkRect x y w h in Position $ V4 rect rect rect rect

initTexture :: MonadIO m => String -> SystemT' m Texture
initTexture path = do
  Renderer ren <- get global
  texture <- SDLI.loadTexture ren path
  return $ Texture texture
