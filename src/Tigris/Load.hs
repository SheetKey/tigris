module Tigris.Load where

-- mylib
import Tigris.ECS
import Tigris.Graphics

-- sdl
import qualified SDL
import qualified SDL.Image as SDLI
import qualified SDL.Font as SDLF

-- apecs
import Apecs

player ::
player = do
  Renderer ren <- get global 
  texture <- SDLI.loadTexture ren "sprites/Sprite-0001.png:
  newEntity_ ( Player
             , Position $ mkRect 10 10 64 64
             , Rotation 0 (2, 2) (V2 False False)
             , RToMouse
             , Texture 
             , Velocity (V2 0 0)
             , SpriteSheet 0 0 4 32 32 1000 0
             )
