-- | A collection of geometric types and pure operations.
module Mutant.Geom where

import Linear (V2 (..))


-- | A rectangle.
data Rect a
  = Rect
  { -- | Upper left
    rectUpperLeft :: V2 a
    -- | Width and height
  , rectExtents   :: V2 a
  }


-- | Inset a rectangle by some amount in x and y.
insetRect
  :: Num a
  => Rect a
  -> V2 a
  -> Rect a
insetRect r v =
  Rect
  { rectUpperLeft = rectUpperLeft r + v
  , rectExtents = rectExtents r - 2 * v
  }


-- | A line.
data Line a
  = Line
  { -- | Line start
    lineStart :: V2 a
    -- | Line end
  , lineEnd   :: V2 a
  }
