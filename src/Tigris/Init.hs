{-# LANGUAGE TypeApplications #-}

module Tigris.Init where

-- mylib
import Tigris.Graphics
import Tigris.ECS

-- sdl
import qualified SDL

-- apecs
import Apecs
import Apecs.Stores

-- base
import Control.Monad.IO.Class

-- text
import Data.Text

initAndRun :: Text -> SystemT' IO () -> IO ()
initAndRun winName gameLoop = do
  SDL.initialize [ SDL.InitVideo, SDL.InitEvents ]
  win <- SDL.createWindow winName windowConfig
  ren <- SDL.createRenderer win (-1) SDL.defaultRenderer
  world <- initWorld
  runWith world $ do
    setReadOnly global $ Window win
    setReadOnly global $ Renderer ren
    gameLoop