{- |
Type synonyms for `SystemT`.
-}

module Tigris.ECS.System
  ( module Tigris.ECS.System
  , Apecs.SystemT (..)
  ) where

-- rhine
import FRP.Rhine

-- apecs
import Apecs

-- mylib
import Tigris.ECS.World

-- base
import Control.Monad.Trans.Reader


type ClSFS m cl a b = ClSF (SystemT World m) cl a b

type ClSFSR r m cl a b = ClSF (ReaderT r (SystemT World m)) cl a b

type SNS m cl a b = SN (SystemT World m) cl a b

type RhineS m cl a b = Rhine (SystemT World m) cl a b

type SystemT' m a = SystemT World m a

type ParClockS m clL clR = ParallelClock (SystemT World m) clL clR

type SeqClockS m cl1 cl2 = SequentialClock (SystemT World m) cl1 cl2

type ReaderT' r m a = ReaderT r (SystemT World m) a
