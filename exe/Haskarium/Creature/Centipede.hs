{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Haskarium.Creature.Centipede
    ( Centipede (..)
    , draw
    , generate
    ) where

import           Graphics.Gloss (Point, circleSolid, color, orange, translate)
import           Haskarium.Class.Drawable (Drawable (..))
import           Haskarium.Class.Generate (Generate (..))
import           Haskarium.Class.Located (Located (..))
import           Haskarium.Creature.Types (Creature (..))
import           Haskarium.Util (randomRS)

newtype Centipede = Centipede{segments :: [Point]}

instance Generate Centipede where
    generate = mkCentipede <$> randomRS (5, 15)
      where
        mkCentipede numSegments =
            Centipede{segments = replicate numSegments (0, 0)}

    turnRateRange = (pi / 25, pi / 20)

instance Drawable (Creature Centipede) where
    draw Creature{position, size, species = Centipede segments} =
        foldMap draw' (position : segments)
      where
        draw' (x, y) =
            translate x y .
            color orange $
            circleSolid size

instance Located (Creature Centipede) where
    getPoints Creature{position, species} = position : segments species
