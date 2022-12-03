-- TODO: Move this entire module to tigris-demo
module Tigris.Load where

-- mylib
import Tigris.ECS
import Tigris.Graphics
import Tigris.Init

-- apecs
import Apecs

-- base
import Control.Monad.IO.Class

--player :: MonadIO m => SystemT' m ()
--player = do
--  --Renderer ren <- get global 
--  --texture <- SDLI.loadTexture ren "sprites/Sprite-0001.png"
--  texture <- initTexture "sprites/Sprite-0001.png"
--  newEntity_ ( Player
--             , initPosition 10 10 64 64
--             , Rotation 0 (2, 2) (V2 False False)
--             , RToMouse
--             , texture
--             , Velocity (V2 0 0)
--             , SpriteSheet 0 0 4 32 32 5 0
--             , ( Speed 250
--               , Health 3
--               )
--             )
--
--enemy :: MonadIO m => SystemT' m ()
--enemy = do
--  --Renderer ren <- get global 
--  --texture <- SDLI.loadTexture ren "sprites/Sprite-0001.png"
--  texture <- initTexture "sprites/Sprite-0001.png"
--  newEntity_ ( initPosition 100 100 64 64
--             , texture
--             , Velocity (V2 0 0)
--             , SpriteSheet 0 0 4 32 32 5 0
--             , Speed 250
--             )
