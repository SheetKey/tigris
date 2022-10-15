{-# LANGUAGE TypeApplications #-}

module Tigris.Init where

-- mylib
import Tigris.Graphics
import Tigrics.ECS

-- sdl
import qualified SDL

-- apecs
import Apecs

init :: MonadIO m => String -> SystemT' m () -> IO ()
init winName gameLoop = do
  SDL.initialize [ SDL.InitVidea, SDL.InitEvents ]
  win <- SDL.createWindow winName windowConfig
  ren <- SDL.createRenderer win (-1) SDL.defaultRenderer
  world <- initWorld
  runWith world $ do
    destroyReadOnly global (Proxy @(SDLWindow, SDLRenderer))
    set global $ SDLWindow win
    set global $ SDLRenderer ren
    setReadOnly global (Proxy @(SDLWindow, SDLRenderer))
    gameLoop
