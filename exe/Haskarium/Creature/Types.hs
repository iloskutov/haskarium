module Haskarium.Creature.Types
    ( Creature (..)
    ) where

import           Graphics.Gloss (Point)
import           Haskarium.Types (Angle, Distance, RadiansPerSecond)

data Creature species = Creature
    { position :: !Point
    , targetDir :: !Angle
    , currentDir :: !Angle
    , turnRate :: !RadiansPerSecond
    , species :: !species
    , size :: !Distance
    }
