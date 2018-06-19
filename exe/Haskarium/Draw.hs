{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Haskarium.Draw
    ( drawCreature
    , triangleBody
    ) where

import           Graphics.Gloss (Picture, polygon, rotate, scale, translate)
import           Graphics.Gloss.Geometry.Angle (radToDeg)

import           Haskarium.Creature.Types

drawCreature :: Picture -> Creature s -> Picture
drawCreature body Creature{position = (x, y), currentDir, size} =
    translate x y $ scale size size $ rotate (- radToDeg currentDir) body

triangleBody :: Picture
triangleBody = polygon
    [ ( 0.5,  0)
    , (-0.5, -0.5)
    , (-0.5,  0.5)
    ]
