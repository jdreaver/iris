-- | Axis visual for use with plots.

module Iris.Visuals.Axis
       ( Axis (..)
       , defaultAxis
       , ticks
       , AxisSpec (..)
       , axisInit
       , makeAxis
       )
       where

import           Data.Fixed (div')
import           Data.Foldable (find)
import           Data.Maybe (fromMaybe)
import qualified Data.Vector.Storable as V
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L

import           Iris.Visuals.Line
import           Iris.SceneGraph

data Axis = Axis
  { axisMin           :: GL.GLfloat
  , axisMax           :: GL.GLfloat
  , axisNiceIntervals :: [GL.GLfloat]
  , axisNumTicks      :: Int
  } deriving (Show)

defaultAxis :: Axis
defaultAxis = Axis 0 1 [1, 2, 2.5, 5, 7.5, 10] 10

-- | Computes the tick locations on our axis. This algorithm is adapted from
-- matplotlib. This can easily be made a user-configurable function in the
-- future.
ticks :: Axis -> [GL.GLfloat]
ticks (Axis vmin vmax intervals nticks) = ticksInRange
  where
    -- Determine the order of magnitude of the tick spacing. We will scale the
    -- other calculations using this.
    range = abs (vmax - vmin)
    orderOfMag = floor $ logBase 10 (range / fromIntegral nticks) :: Int
    scale = 10 ** fromIntegral orderOfMag

    -- We eventually need to compute what a "nicer" spacing is. First we
    -- compute the raw spacing, which is what the spacing would be with
    -- perfectly even tick marks.
    rawSpacing = range / fromIntegral nticks

    -- Now, we can find which "nice" spacing is closest to this raw spacing. We
    -- need it to fill the whole interval, so we find the first nice spacing
    -- that does so. (Note that `maybeSpacing` should never be Nothing, but we
    -- want to be type-safe with our use of `find`.)
    scaledSteps = map (* scale) intervals
    maybeSpacing = find (>= rawSpacing) scaledSteps
    spacing = fromMaybe (10 * scale) maybeSpacing

    -- A "better" minimum is one that is divisible by our spacing.
    betterMin = spacing * fromIntegral (div' vmin spacing :: Int)

    -- Compute the ticks, and then filter out ticks not in our range. Note that
    -- allTicks has (nticks + 1) ticks. This is in case out ticks line up
    -- perfectly with the axis range, in which case we'll get ticks on the
    -- endpoints.
    allTicks = fmap ((+) betterMin . (*) spacing . fromIntegral) [0..nticks]
    ticksInRange = filter (\x -> x >= vmin && x <= vmax) allTicks


data AxisSpec = AxisSpec
  { axisSpecAxis :: !Axis
  , axisSpecPos  :: !(L.V2 GL.GLfloat)
  , axisSpecSize :: !(L.V2 GL.GLfloat)
  } deriving (Show)


axisInit :: AxisSpec -> IO DrawNode
axisInit spec =
  do item <- makeAxis spec
     return $ DrawNode (drawLine item)

makeAxis :: AxisSpec -> IO LineItem
makeAxis (AxisSpec axis (L.V2 px py) (L.V2 w h)) =
  do let ticks' = ticks axis
         widthFactor = w / (axisMax axis - axisMin axis)
         ticksNorm = map (\t -> px + (t - axisMin axis) * widthFactor) ticks'
         tickVerts = map (\t -> [L.V3 t py 0, L.V3 t (py - h) 0, L.V3 t py 0]) ticksNorm
         lineVerts = concat $ [L.V3 px py 0] : tickVerts ++ [[L.V3 (px + w) py 0]]
     makeLine $ LineSpec (V.fromList lineVerts) (L.V3 1 1 1)
