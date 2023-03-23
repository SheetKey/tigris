{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
A global store using `TQueue` that does not block when
the queue is empty. 
`TQueue` should be used in situations when data
is written faster than it is processed, but where
old values are still important and should be processed
in order.
-}
module Tigris.ECS.Stores.TQGlobal where

-- stm
import Control.Monad.STM
import Control.Concurrent.STM.TQueue

-- apecs
import Apecs
import Apecs.Core

-- base
import Control.Monad.IO.Class

-- | A global component store using `TQueue`.
newtype TQGlobal c = TQGlobal (TQueue c)
type instance Elem (TQGlobal c) = Maybe c
instance MonadIO m => ExplInit m (TQGlobal c) where
  {-# INLINE explInit #-}
  explInit = liftIO $ TQGlobal <$> newTQueueIO

instance MonadIO m => ExplGet m (TQGlobal c) where
  {-# INLINE explGet #-}
  explGet (TQGlobal tqueue) _ = liftIO $ atomically $ tryReadTQueue tqueue
  {-# INLINE explExists #-}
  explExists (TQGlobal tqueue) _ = liftIO $ atomically $ not <$> isEmptyTQueue tqueue

instance MonadIO m => ExplSet m (TQGlobal c) where
  {-# INLINE explSet #-}
  explSet (TQGlobal tqueue) _ c = liftIO $ atomically $ writeTQueue tqueue c
