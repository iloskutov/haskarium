{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Haskarium.Creature.Ant
    ( Ant (..)
    , draw
    , generate
    ) where

import           Data.Monoid ((<>))
import           Graphics.Gloss (circle, color, red, translate)
import           Haskarium.Class.Drawable (Drawable (..))
import           Haskarium.Class.Generate (Generate (..))
import           Haskarium.Class.Located (Located (..))
import           Haskarium.Creature.Types (Creature (..))
import           Haskarium.Draw (drawCreature, triangleBody)

data Ant = Ant

instance Generate Ant where
    generate = pure Ant

instance Drawable (Creature Ant) where
    draw =
        drawCreature $ color red $ triangleBody <> translate (-0.5) 0 (circle 0.5)

instance Located (Creature Ant) where
    getPoints Creature{position} = [position]
