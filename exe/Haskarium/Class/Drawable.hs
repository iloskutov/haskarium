module Haskarium.Class.Drawable
    ( Drawable (..)
    ) where

import           Graphics.Gloss (Picture)

class Drawable a where
    draw :: a -> Picture
