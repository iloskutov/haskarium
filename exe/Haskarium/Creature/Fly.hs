{-# LANGUAGE FlexibleInstances #-}

module Haskarium.Creature.Fly
    ( Fly (..)
    , draw
    , generate
    ) where

import           Data.Monoid ((<>))
import           Graphics.Gloss (circle, color, green, translate)
import           Haskarium.Class.Drawable (Drawable (..))
import           Haskarium.Class.Generate (Generate (..))
import           Haskarium.Creature.Types (Creature (..))
import           Haskarium.Draw (drawCreature, triangleBody)

data Fly = Fly

instance Generate Fly where
    generate = pure Fly

instance Drawable (Creature Fly) where
    draw =
        drawCreature $
        color green $
        triangleBody <> translate 0.5 0.5 (circle 0.5) <> translate 0.5 (-0.5) (circle 0.5)
