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

modSizeV :: SDL.V2 SDL.CInt -> SDL.Rectangle SDL.CInt -> SDL.Rectangle SDL.CInt
modSizeV (SDL.V2 w h) rect = modSize w h rect

modPntV :: SDL.V2 SDL.CInt -> SDL.Rectangle SDL.CInt -> SDL.Rectangle SDL.CInt
modPntV (SDL.V2 x y) rect = modPnt x y rect

intersectRects :: SDL.Rectangle SDL.CInt -> SDL.Rectangle SDL.CInt -> Bool
intersectRects (SDL.Rectangle (SDL.P (SDL.V2 x1 y1)) (SDL.V2 w1 h1))
  (SDL.Rectangle (SDL.P (SDL.V2 x2 y2)) (SDL.V2 w2 h2))
  = x1 + w1 >= x2 &&
    x2 + w2 >= x1 &&
    y1 + h1 >= y2 &&
    y2 + h2 >= y1

setXV2 :: SDL.CInt -> SDL.V2 SDL.CInt -> SDL.V2 SDL.CInt
setXV2 x (SDL.V2 _ y) = SDL.V2 x y

setYV2 :: SDL.CInt -> SDL.V2 SDL.CInt -> SDL.V2 SDL.CInt
setYV2 y (SDL.V2 x _) = SDL.V2 x y

getXV2 :: SDL.V2 SDL.CInt -> SDL.CInt
getXV2 (SDL.V2 x _) = x

getYV2 :: SDL.V2 SDL.CInt -> SDL.CInt
getYV2 (SDL.V2 _ y) = y
