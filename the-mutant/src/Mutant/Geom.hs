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
  } deriving (Show, Eq)


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


-- | Results in True if the Rect contains the given point.
rectContainsPoint
  :: (Num a, Ord a)
  => Rect a
  -> V2 a
  -> Bool
rectContainsPoint r (V2 px py) =
  let V2 minx miny = rectUpperLeft r
      V2 maxx maxy = rectUpperLeft r + rectExtents r
  in (px >= minx && px <= maxx) && (py >= miny && py <= maxy)


-- | A line.
data Line a
  = Line
  { -- | Line start
    lineStart :: V2 a
    -- | Line end
  , lineEnd   :: V2 a
  }
