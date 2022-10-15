{- |
A global component store using `STM` and `TMVar`.
`Apecs.get` will read the value and
not empty the `TMVar`, so `Apecs.get` will
not block. `Apecs.set` writes over
any currently held value.
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Tigris.ECS.Stores.TMVGlobal where

-- stm
import Control.Monad.STM
import Control.Concurrent.STM.TMVar

-- apecs
import Apecs
import Apecs.Core

-- base
import Control.Monad.IO.Class

-- | A global component store using `TMVar`.
newtype TMVGlobal c = TMVGlobal (TMVar c)
type instance Elem (TMVGlobal c) = c
instance MonadIO m => ExplInit m (TMVGlobal c) where
  {-# INLINE explInit #-}
  explInit = liftIO $ TMVGlobal <$> newEmptyTMVarIO

instance MonadIO m => ExplGet m (TMVGlobal c) where
  {-# INLINE explGet #-}
  explGet (TMVGlobal tmvar) _ = (\case {Just a -> a; Nothing -> error "'TMVarGlobal' is empty for some component. Thread blocked indefinitely in STM transaction."}) <$> (liftIO $ atomically $ tryReadTMVar tmvar)
  {-# INLINE explExists #-}
  explExists (TMVGlobal tmvar) _ = liftIO $ atomically $ not <$> isEmptyTMVar tmvar

instance MonadIO m => ExplSet m (TMVGlobal c) where
  {-# INLINE explSet #-}
  explSet (TMVGlobal tmvar) _ c = liftIO $ atomically $ tryTakeTMVar tmvar >> putTMVar tmvar c
