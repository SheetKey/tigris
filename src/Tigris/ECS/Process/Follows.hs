module Tigris.ECS.Process.Follows where

-- tigris
import Tigris.ECS.Components
import Tigris.ECS.System

-- apecs
import Apecs

-- rhine
import FRP.Rhine hiding (get)

 
 

_follow :: MonadIO m => SystemT' m ()
_follow = cmapM $ \(Follows _id offset) -> do
  Position fpos <- get $ Entity _id
  return $ Position $ (+ offset) <$> fpos

follow :: MonadIO m => ClSFS m cl () ()
follow = constMCl _follow
