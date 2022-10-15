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


type ClSFS m cl a b = ClSF (SystemT World m) cl a b

type SNS m cl a b = SN (SystemT World m) cl a b

type RhineS m cl a b = Rhine (SystemT World m) cl a b

type SystemT' m a = SystemT World m a

type ParClockS m clL clR = ParallelClock (SystemT World m) clL clR

type SeqClockS m cl1 cl2 = SequentialClock (SystemT World m) cl1 cl2
