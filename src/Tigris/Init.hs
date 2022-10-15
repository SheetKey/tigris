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

-- base
import Control.Monad.IO.Class

-- text
import Data.Text

initAndRun :: Text -> SystemT' IO () -> IO ()
initAndRun winName gameLoop = do
  --SDL.initialize [ SDL.InitVideo, SDL.InitEvents, SDL.InitGameController ]
  SDL.initializeAll
  SDLI.initialize [ SDLI.InitPNG ]
  SDLF.initialize
  win <- SDL.createWindow winName windowConfig
  ren <- SDL.createRenderer win (-1) SDL.defaultRenderer
  p <- SDL.getAbsoluteMouseLocation
  liftIO $ print p
  world <- initWorld
  runWith world $ do
    setReadOnly global $ Window win
    setReadOnly global $ Renderer ren
    gameLoop

testInit :: IO ()
testInit = do
  SDL.initializeAll
  win <- SDL.createWindow "test" SDL.defaultWindow
  ren <- SDL.createRenderer win (-1) SDL.defaultRenderer
  SDL.delay 2000
  p <- SDL.getAbsoluteMouseLocation
  liftIO $ print p
