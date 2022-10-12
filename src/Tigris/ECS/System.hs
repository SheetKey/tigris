module Tigris.ECS.System where

-- rhine
import FRP.Rhine

-- apecs
import Apecs

-- mylib
import Tigris.ECS.World


type ClSFS m cl a b = ClSF (SystemT World m) cl a b

type SNS m cl a b = SN (SystemT World m) cl a b

type RhineS m cl a b = Rhine (SystemT World m) cl a b
