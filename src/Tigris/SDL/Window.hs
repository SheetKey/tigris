{- |
Things related to `Window` for `SDL`.
-}

module Tigris.SDL.Window where

import qualified SDL

-- | Default window configuration. 
windowConfig :: SDL.WindowConfig
windowConfig = SDL.WindowConfig
  { SDL.windowBorder          = True
  , SDL.windowHighDPI         = False
  , SDL.windowInputGrabbed    = False
  , SDL.windowMode            = SDL.Windowed
  , SDL.windowGraphicsContext = SDL.OpenGLContext $
                                SDL.defaultOpenGL
                                { SDL.glProfile = SDL.Core SDL.Normal 3 3  -- version
                                , SDL.glMultisampleSamples = 4             -- MSAA
                                }
  , SDL.windowPosition        = SDL.Wherever
  , SDL.windowResizable       = False
  , SDL.windowInitialSize     = SDL.V2 800 600
  , SDL.windowVisible         = True
  }
