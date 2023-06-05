{-# LANGUAGE LambdaCase #-}
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
type instance Elem (TQGlobal c) = c
instance (MonadIO m, Monoid c) => ExplInit m (TQGlobal c) where
  {-# INLINE explInit #-}
  explInit = liftIO $ TQGlobal <$> newTQueueIO

instance (MonadIO m, Monoid c) => ExplGet m (TQGlobal c) where
  {-# INLINE explGet #-}
  explGet (TQGlobal tqueue) _ = fmap (\case { Nothing -> mempty; Just c -> c}) (liftIO $ atomically $ tryReadTQueue tqueue)
  {-# INLINE explExists #-}
  explExists (TQGlobal tqueue) _ = liftIO $ atomically $ not <$> isEmptyTQueue tqueue

instance (MonadIO m, Monoid c) => ExplSet m (TQGlobal c) where
  {-# INLINE explSet #-}
  explSet (TQGlobal tqueue) _ c = liftIO $ atomically $ writeTQueue tqueue c
