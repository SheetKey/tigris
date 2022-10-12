module Tigris.Graphics.Rectangle where

-- sdl
import qualified SDL

-- cint
import Foreign.C.Types as SDL


mkRect :: SDL.CInt -> SDL.CInt -> SDL.CInt -> SDL.CInt -> SDL.Rectangle SDL.CInt
mkRect x y w h = SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w h)

modRect :: SDL.CInt -> SDL.CInt -> SDL.CInt -> SDL.CInt -> SDL.Rectangle SDL.CInt -> SDL.Rectangle SDL.CInt
modRect x y w h (SDL.Rectangle (SDL.P (SDL.V2 x1 y1)) (SDL.V2 w1 h1)) = 
  SDL.Rectangle (SDL.P (SDL.V2 (x+x1) (y+y1))) (SDL.V2 (w+w1) (h+h1))

modPnt :: SDL.CInt -> SDL.CInt -> SDL.Rectangle SDL.CInt -> SDL.Rectangle SDL.CInt
modPnt x y rect = modRect x y 0 0 rect

modSize :: SDL.CInt -> SDL.CInt -> SDL.Rectangle SDL.CInt -> SDL.Rectangle SDL.CInt
modSize w h rect = modRect 0 0 w h rect

modPntV :: SDL.V2 SDL.CInt -> SDL.Rectangle SDL.CInt -> SDL.Rectangle SDL.CInt
modPntV (SDL.V2 x y) rect = modPnt x y rect
