{-# LANGUAGE AllowAmbiguousTypes #-}

module Haskarium.Class.Generate
    ( Generate(..)
    ) where

import           Control.Monad.State.Strict (State)
import           System.Random (StdGen)

import           Haskarium.Types (Angle)

class Generate a where
    generate :: State StdGen a

    turnRateRange :: (Angle, Angle)
    turnRateRange = (pi / 4, pi / 2)
