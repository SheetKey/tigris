{- |
Utilites for working with `Rectangle`, `Point`, and `V2` types
provided by `SDL`.
-}

module Tigris.Graphics.Rectangle where

-- sdl
import qualified SDL

-- cint
import Foreign.C.Types as SDL

-- | Utility to constuct a rectangle.
mkRect :: SDL.CInt -> SDL.CInt -> SDL.CInt -> SDL.CInt -> SDL.Rectangle SDL.CInt
mkRect x y w h = SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w h)

-- | Utility to modify a rectangle.
modRect :: SDL.CInt -> SDL.CInt -> SDL.CInt -> SDL.CInt -> SDL.Rectangle SDL.CInt -> SDL.Rectangle SDL.CInt
modRect x y w h (SDL.Rectangle (SDL.P (SDL.V2 x1 y1)) (SDL.V2 w1 h1)) = 
  SDL.Rectangle (SDL.P (SDL.V2 (x+x1) (y+y1))) (SDL.V2 (w+w1) (h+h1))

-- | Utility to modify a rectangle.
modPnt :: SDL.CInt -> SDL.CInt -> SDL.Rectangle SDL.CInt -> SDL.Rectangle SDL.CInt
modPnt x y rect = modRect x y 0 0 rect

-- | Utility to modify a rectangle.
modSize :: SDL.CInt -> SDL.CInt -> SDL.Rectangle SDL.CInt -> SDL.Rectangle SDL.CInt
modSize w h rect = modRect 0 0 w h rect

-- | Utility to modify a rectangle.
modSizeV :: SDL.V2 SDL.CInt -> SDL.Rectangle SDL.CInt -> SDL.Rectangle SDL.CInt
modSizeV (SDL.V2 w h) rect = modSize w h rect

-- | Utility to modify a rectangle.
modPntV :: SDL.V2 SDL.CInt -> SDL.Rectangle SDL.CInt -> SDL.Rectangle SDL.CInt
modPntV (SDL.V2 x y) rect = modPnt x y rect

-- | Determine if two rectangles intersect.
intersectRects :: SDL.Rectangle SDL.CInt -> SDL.Rectangle SDL.CInt -> Bool
intersectRects (SDL.Rectangle (SDL.P (SDL.V2 x1 y1)) (SDL.V2 w1 h1))
  (SDL.Rectangle (SDL.P (SDL.V2 x2 y2)) (SDL.V2 w2 h2))
  = x1 + w1 >= x2 &&
    x2 + w2 >= x1 &&
    y1 + h1 >= y2 &&
    y2 + h2 >= y1

-- | Utility to change x component of a vector.
setXV2 :: SDL.CInt -> SDL.V2 SDL.CInt -> SDL.V2 SDL.CInt
setXV2 x (SDL.V2 _ y) = SDL.V2 x y

-- | Utility to change y component of a vector.
setYV2 :: SDL.CInt -> SDL.V2 SDL.CInt -> SDL.V2 SDL.CInt
setYV2 y (SDL.V2 x _) = SDL.V2 x y

-- | Utility to get the x component of a vector.
getXV2 :: SDL.V2 SDL.CInt -> SDL.CInt
getXV2 (SDL.V2 x _) = x

-- | Utility to get the y component of a vector.
getYV2 :: SDL.V2 SDL.CInt -> SDL.CInt
getYV2 (SDL.V2 _ y) = y
