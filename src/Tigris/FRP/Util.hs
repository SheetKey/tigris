{- |
Utilities for working with signal functions.
-}

{-# LANGUAGE Arrows #-}

module Tigris.FRP.Util where

import FRP.Rhine

-- | A `ClSF` that duplicates the input.
splitInput :: Monad m => ClSF m cl a (a, a)
splitInput = proc a -> do returnA -< (a, a)

-- | A `ClSF` that joins the input. Typically post-composed with parallel composed signal functions that both output `()`.
joinOutput :: (Monad m, Semigroup a) => ClSF m cl (a, a) a
joinOutput = proc (a1, a2) -> do returnA -< a1 <> a2
