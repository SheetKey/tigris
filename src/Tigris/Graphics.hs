{-# LANGUAGE PatternSynonyms #-}

module Tigris.Graphics
  ( module X
  , SDL.CInt (..)
  , SDL.CDouble (..)
  , SDL.Rectangle (..)
  , SDL.Point (..)
  , SDL.V2 (..)
  , SDL.normalize
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
  , SDL.InputMotion (..)
  , SDL.Keysym (..)
  , SDL.Keycode (..)
  , pattern SDL.KeycodeW
  , pattern SDL.KeycodeA
  , pattern SDL.KeycodeS
  , pattern SDL.KeycodeD
    
  , SDL.windowSizeChangedEventSize

  , SDL.copyEx
  , SDL.copy
  ) where

-- SDL exports
import qualified SDL

-- cint
import Foreign.C.Types as SDL
  
-- my lib
import Tigris.Graphics.Rectangle as X
import Tigris.Graphics.Window as X
