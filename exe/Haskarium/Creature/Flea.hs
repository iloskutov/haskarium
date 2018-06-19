{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Haskarium.Creature.Flea
    ( Flea (..)
    , draw
    , generate
    ) where

import           Data.Monoid ((<>))
import           Graphics.Gloss (blue, circle, color, translate)
import           Haskarium.Class.Drawable (Drawable (..))
import           Haskarium.Class.Generate (Generate (..))
import           Haskarium.Class.Located (Located (..))
import           Haskarium.Creature.Types (Creature (..))
import           Haskarium.Draw (drawCreature, triangleBody)
import           Haskarium.Types (Time)
import           Haskarium.Util (randomRS)

newtype Flea = Flea{idleTime :: Time}

instance Generate Flea where
    generate = Flea <$> randomRS (0.0, 1.0)

instance Drawable (Creature Flea) where
    draw =
        drawCreature $ color blue $ triangleBody <> translate (-0.5) 0 (circle 0.5)

instance Located (Creature Flea) where
    getPoints Creature{position} = [position]
