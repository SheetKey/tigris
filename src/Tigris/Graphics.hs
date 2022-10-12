module Tigris.Graphics
  ( module X
  , SDL.CInt (..)
  , SDL.CDouble (..)
  , SDL.Rectangle (..)
  , SDL.Point (..)
  , SDL.V2 (..)
  , SDL.Texture

  , SDL.getAbsoluteMouseLocation
  ) where

-- SDL exports
import qualified SDL

-- cint
import Foreign.C.Types as SDL
  
-- my lib
import Tigris.Graphics.Rectangle as X
