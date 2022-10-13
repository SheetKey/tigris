module Tigris.Graphics
  ( module X
  , SDL.CInt (..)
  , SDL.CDouble (..)
  , SDL.Rectangle (..)
  , SDL.Point (..)
  , SDL.V2 (..)
  , SDL.Texture

  , SDL.getAbsoluteMouseLocation

  , SDL.Window
  , SDL.Renderer
  , SDL.get
  , SDL.windowSize

  , SDL.Event (..)
  , SDL.EventPayload (..)
  , SDL.pollEvents
  , SDL.pollEvent
  , SDL.KeyboardEventData (..)
  , SDL.keysymKeycode
  , SDL.Keycode (..)
  , SDL.windowSizeChangedEventSize
  ) where

-- SDL exports
import qualified SDL

-- cint
import Foreign.C.Types as SDL
  
-- my lib
import Tigris.Graphics.Rectangle as X
import Tigris.Graphics.Window as X
