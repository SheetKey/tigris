{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Tigris.ECS.Stores.BTMGlobal where

-- stm
import Control.Monad.STM
import Control.Concurrent.STM.TMVar

-- apecs
import Apecs
import Apecs.Core

-- base
import Control.Monad.IO.Class


newtype BTMGlobal c = BTMGlobal (TMVar c)
type instance Elem (BTMGlobal c) = c
instance MonadIO m => ExplInit m (BTMGlobal c) where
  {-# INLINE explInit #-}
  explInit = liftIO $ BTMGlobal <$> newEmptyTMVarIO

instance MonadIO m => ExplGet m (BTMGlobal c) where
  {-# INLINE explGet #-}
  explGet (BTMGlobal tmvar) _ = liftIO $ atomically $ takeTMVar tmvar
  {-# INLINE explExists #-}
  explExists (BTMGlobal tmvar) _ = liftIO $ atomically $ not <$> isEmptyTMVar tmvar

instance MonadIO m => ExplSet m (BTMGlobal c) where
  {-# INLINE explSet #-}
  explSet (BTMGlobal tmvar) _ c = liftIO $ atomically $ tryTakeTMVar tmvar >> putTMVar tmvar c
