{- |
A version of `TMVGlobal` that blocks
when getting values. This is meant to
be used for creating clocks. A clock
will tick on `Apecs.get`, which will block
if there is no value. A signal function
can provide a value with `Apecs.set`.
`Apecs.set` writes over any currently held value.
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Tigris.ECS.Stores.BTMVGlobal where

-- stm
import Control.Monad.STM
import Control.Concurrent.STM.TMVar

-- apecs
import Apecs
import Apecs.Core

-- base
import Control.Monad.IO.Class

-- | A global component store using `TMVar` that blocks
--   on `Apecs.get`.
newtype BTMVGlobal c = BTMVGlobal (TMVar c)
type instance Elem (BTMVGlobal c) = c
instance MonadIO m => ExplInit m (BTMVGlobal c) where
  {-# INLINE explInit #-}
  explInit = liftIO $ BTMVGlobal <$> newEmptyTMVarIO

instance MonadIO m => ExplGet m (BTMVGlobal c) where
  {-# INLINE explGet #-}
  explGet (BTMVGlobal tmvar) _ = liftIO $ atomically $ takeTMVar tmvar
  {-# INLINE explExists #-}
  explExists (BTMVGlobal tmvar) _ = liftIO $ atomically $ not <$> isEmptyTMVar tmvar

instance MonadIO m => ExplSet m (BTMVGlobal c) where
  {-# INLINE explSet #-}
  explSet (BTMVGlobal tmvar) _ c = liftIO $ atomically $ tryTakeTMVar tmvar >> putTMVar tmvar c
