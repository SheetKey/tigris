{-# LANGUAGE Arrows #-}

module Tigris.FRP.Util where

import FRP.Rhine

splitInput :: Monad m => ClSF m cl a (a, a)
splitInput = proc a -> do returnA -< (a, a)

joinOutput :: (Monad m, Semigroup a) => ClSF m cl (a, a) a
joinOutput = proc (a1, a2) -> do returnA -< a1 <> a2
