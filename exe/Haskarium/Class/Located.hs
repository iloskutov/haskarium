module Haskarium.Class.Located
    ( Located(..)
    ) where

import           Graphics.Gloss (Point)

class Located a where
    getPoints :: a -> [Point]
