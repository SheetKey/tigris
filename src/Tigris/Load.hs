module Tigris.Load where

-- mylib
import Tigris.ECS
import Tigris.Graphics

-- sdl
import qualified SDL.Image as SDLI

-- apecs
import Apecs

-- base
import Control.Monad.IO.Class

player :: MonadIO m => SystemT' m ()
player = do
  Renderer ren <- get global 
  texture <- SDLI.loadTexture ren "sprites/Sprite-0001.png"
  newEntity_ ( Player
             , Position $ mkRect 10 10 64 64
             , Rotation 0 (2, 2) (V2 False False)
             , RToMouse
             , Texture texture
             , Velocity (V2 0 0)
             , SpriteSheet 0 0 4 32 32 5 0
             , Speed 250
             )

enemy :: MonadIO m => SystemT' m ()
enemy = do
  Renderer ren <- get global 
  texture <- SDLI.loadTexture ren "sprites/Sprite-0001.png"
  newEntity_ ( Position $ mkRect 30 30 64 64
             , Texture texture
             , Velocity (V2 0 0)
             , SpriteSheet 0 0 4 32 32 5 0
             , Speed 250
             )
