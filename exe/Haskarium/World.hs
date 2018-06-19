{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Haskarium.World
  ( Sim
    , World (..)
    , Located (..)
    , LandCreature(..)
    , landCreatures
    , draw
    , worldOf
  ) where

import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.RWS.Strict (RWS)
import           Data.Monoid ((<>))
import           Haskarium.Class.Drawable
import           Haskarium.Class.Located
import           Haskarium.Creature.Ant (Ant)
import           Haskarium.Creature.Centipede (Centipede)
import           Haskarium.Creature.Flea (Flea)
import           Haskarium.Creature.Fly (Fly)
import           Haskarium.Creature.Types (Creature (..))
import           System.Random (StdGen)

data World = World
    { ants :: ![Creature Ant]
    , centipedes :: ![Creature Centipede]
    , fleas :: ![Creature Flea]
    , flies :: ![Creature Fly]
    }

type Sim a = RWS World () StdGen a

instance Drawable World where
    draw World{ants, centipedes, fleas, flies} =
        foldMap draw ants <>
        foldMap draw centipedes <>
        foldMap draw fleas <>
        foldMap draw flies

worldOf :: MonadReader World m => (World -> a) -> m a
worldOf = asks

data LandCreature
    = forall species
    . Located (Creature species)
    => LandCreature (Creature species)

landCreatures :: World -> [LandCreature]
landCreatures World{ants, centipedes, fleas} = mconcat
    [ map LandCreature ants
    , map LandCreature centipedes
    , map LandCreature fleas
    ]
