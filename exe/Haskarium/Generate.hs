{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Haskarium.Generate
    ( makeGame
    ) where

import           Control.Monad (replicateM)
import           Control.Monad.State.Strict (State)
import           Graphics.Gloss (Point)
import           Haskarium.Class.Generate (Generate (..))
import           Haskarium.Const (creatureSizeMax, creatureSizeMin)
import           Haskarium.Creature.Types (Creature (..))
import           Haskarium.Util (randomRS)
import           Haskarium.World (World (..))
import           Numeric.Natural (Natural)
import           System.Random (StdGen)

type Window = (Point, Point)

makeGame :: Window -> State StdGen World
makeGame window = World
    <$> makeCreatures window 0 10
    <*> makeCreatures window 0 10
    <*> makeCreatures window 0 10
    <*> makeCreatures window 0 10

makeCreatures
    :: forall species.
    Generate species => Window -> Natural -> Natural -> State StdGen [Creature species]
makeCreatures window minN maxN =
    pNCreatures >>= \nCreatures ->
    replicateM nCreatures makeCreature
  where
    ((minX, minY), (maxX, maxY)) = window
    pNCreatures = randomRS (fromIntegral minN, fromIntegral maxN)
    makeCreature = Creature
        <$> pPos
        <*> pDir
        <*> pDir
        <*> pTR
        <*> generate
        <*> creatureSize
      where
        creatureSize = randomRS (creatureSizeMin, creatureSizeMax)
        px = randomRS (minX + creatureSizeMax / 2, maxX - creatureSizeMax / 2)
        py = randomRS (minY + creatureSizeMax / 2, maxY - creatureSizeMax / 2)
        pPos = (,) <$> px <*> py
        pDir = randomRS (0, 2 * pi)
        pTR = randomRS (turnRateRange @species)
