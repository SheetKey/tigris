{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
A global store using `TQueue` that blocks when
the queue is empty. This is meant to be used for
creating clocks similar to `BTMVGlobal`.
The key difference is that `BTMVGlobal` assumes
that only the most recent value is important.
`BTMVGlobal` deletes old values that have not yet
been processed.
`TQueue` should be used in situations when data
is written faster than it is processed, but where
old values are still important and should be processed
in order.
-}
module Tigris.ECS.Stores.BTQGlobal where

-- stm
import Control.Monad.STM
import Control.Concurrent.STM.TQueue

-- apecs
import Apecs
import Apecs.Core

-- base
import Control.Monad.IO.Class

-- | A global component store using `TQueue` than blocks
--   on `Apecs.get`.
newtype BTQGlobal c = BTQGlobal (TQueue c)
type instance Elem (BTQGlobal c) = c
instance MonadIO m => ExplInit m (BTQGlobal c) where
  {-# INLINE explInit #-}
  explInit = liftIO $ BTQGlobal <$> newTQueueIO

instance MonadIO m => ExplGet m (BTQGlobal c) where
  {-# INLINE explGet #-}
  explGet (BTQGlobal tqueue) _ = liftIO $ atomically $ readTQueue tqueue
  {-# INLINE explExists #-}
  explExists (BTQGlobal tqueue) _ = liftIO $ atomically $ not <$> isEmptyTQueue tqueue

instance MonadIO m => ExplSet m (BTQGlobal c) where
  {-# INLINE explSet #-}
  explSet (BTQGlobal tqueue) _ c = liftIO $ atomically $ writeTQueue tqueue c
