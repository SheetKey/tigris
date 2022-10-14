{- |
Things related to `Window` for `SDL`.
-}

module Tigris.Graphics.Window where

import qualified SDL

-- | Default window configuration. 
windowConfig :: SDL.WindowConfig
windowConfig = SDL.WindowConfig
  { SDL.windowBorder          = True
  , SDL.windowHighDPI         = False
  , SDL.windowInputGrabbed    = False
  , SDL.windowMode            = SDL.FullscreenDesktop
  , SDL.windowGraphicsContext = SDL.NoGraphicsContext
  , SDL.windowPosition        = SDL.Wherever
  , SDL.windowResizable       = False
  , SDL.windowInitialSize     = SDL.V2 800 600
  , SDL.windowVisible         = True
  }
