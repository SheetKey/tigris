{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Tigris.ECS.Stores.TMGlobal where

-- stm
import Control.Monad.STM
import Control.Concurrent.STM.TMVar

-- apecs
import Apecs
import Apecs.Core

-- base
import Control.Monad.IO.Class


newtype TMGlobal c = TMGlobal (TMVar c)
type instance Elem (TMGlobal c) = c
instance MonadIO m => ExplInit m (TMGlobal c) where
  {-# INLINE explInit #-}
  explInit = liftIO $ TMGlobal <$> newEmptyTMVarIO

instance MonadIO m => ExplGet m (TMGlobal c) where
  {-# INLINE explGet #-}
  explGet (TMGlobal tmvar) _ = liftIO $ atomically $ readTMVar tmvar
  {-# INLINE explExists #-}
  explExists (TMGlobal tmvar) _ = liftIO $ atomically $ not <$> isEmptyTMVar tmvar

instance MonadIO m => ExplSet m (TMGlobal c) where
  {-# INLINE explSet #-}
  explSet (TMGlobal tmvar) _ c = liftIO $ atomically $ tryTakeTMVar tmvar >> putTMVar tmvar c
