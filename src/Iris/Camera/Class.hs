-- | Class definition and utilities for all cameras

module Iris.Camera.Class
       ( Camera (..)
       , PressedButtons
       , pressedButtons
       , recordButtons
       , recordClick
       , mouseDragEvents
       , MouseDrag (..)
       , MouseDragBegin (..)
       , filterClipE
       , filterClipB
       ) where

import qualified Data.Map.Strict as Map

import           Iris.Backends
import           Iris.Mouse
import           Iris.OpenGL (Viewport (..), insideViewport)
import           Iris.Reactive
import           Iris.Transformation

-- | Type class for all cameras.
class Camera a where
  cameraTrans    :: a -> Transformation
  camFromEventsB :: a -> CanvasEvents -> MomentIO (Behavior Transformation)


-- | Stores currently pressed buttons and the mouse coordinates when they were
-- pressed.
type PressedButtons = Map.Map MouseButton MousePosition

-- | Default constructor for `PressedButtons`
pressedButtons :: PressedButtons
pressedButtons = Map.fromList []


-- | Creates an Event that holds the currently pressed buttons.
recordButtons :: Behavior MousePosition
              -> Event MouseButtonEvent
              -> Moment (Event PressedButtons)
recordButtons posB mouseE = accumE pressedButtons eClickedCam
  where applyClick p (b, bs) = recordClick p b bs
        eClickedCam = applyClick <$> posB <@> mouseE


-- | Record when a button is pressed in the `PressedButtons` state.
recordClick :: MousePosition ->
               MouseButton ->
               MouseButtonState ->
               PressedButtons ->
               PressedButtons
recordClick p button Pressed bmap = Map.insert button p bmap
recordClick _ button Released bmap = Map.delete button bmap


-- | Constructs events that fires whenever the mouse is dragged. A mouse drag
-- begins when any mouse button is pressed, and the mouse is moved. While the
-- drag is happening, any other button can be pressed too, but this doesn't
-- "restart" the drag.
--
-- The MouseDragBegin event occurs at the start of a drag.
mouseDragEvents :: Event MouseButtonEvent
                -> Observable MousePosition
                -> Moment (Event MouseDragBegin, Event MouseDrag)
mouseDragEvents buttonE (Observable posB posE) =
  do pressedButtonsE <- recordButtons posB buttonE
     pressedButtonsB <- stepper Map.empty pressedButtonsE
     let updateDragE = updateMouseDrag <$> pressedButtonsB <@> posE
     dragMaybeE  <- accumE Nothing updateDragE
     startMaybeE <- mapSuccE (curry dragStart) Nothing dragMaybeE
     return (filterJust startMaybeE, filterJust dragMaybeE)

-- | Container to hold information about a mouse drag event.
data MouseDrag = MouseDrag
  { mouseDragOriginPos  :: !MousePosition
  , mouseDragCurrentPos :: !MousePosition
  , mouseDragButtons    :: ![MouseButton]
  } deriving (Show, Ord, Eq)

-- | Container for info about the beginning of a mouse drag.
data MouseDragBegin = MouseDragBegin
  { dragBeginPosition :: !MousePosition
  , dragBeginButtons  :: [MouseButton]
  } deriving (Show)

updateMouseDrag :: PressedButtons
                -> MousePosition
                -> Maybe MouseDrag -- ^ Current drag state; Nothing = no drag
                -> Maybe MouseDrag
updateMouseDrag pbs pos maybeDrag = if null buttons then Nothing else newDrag
  where buttons  = map fst $ Map.toList pbs
        buttons' = maybe buttons mouseDragButtons maybeDrag
        origin   = maybe pos mouseDragOriginPos maybeDrag
        newDrag  = Just $ MouseDrag origin pos buttons'

dragStart :: (Maybe MouseDrag, Maybe MouseDrag)
          -> Maybe MouseDragBegin
dragStart (Nothing, Just (MouseDrag pos _ bn)) = Just $ MouseDragBegin pos bn
dragStart _ = Nothing

-- | Filters an event by checking if the mouse position is inside a given
-- viewport.
filterClipE :: Behavior Viewport
           -> (a -> MousePosition)
           -> Event a
           -> Event a
filterClipE viewportB toMousePos event = event'
  where toXY (MousePosition x y) = (x, y)
        insideF vp x = uncurry (insideViewport vp) (toXY $ toMousePos x)
        event' = filterApply (insideF <$> viewportB) event


-- | Like filterClipE, except we take an outside Behavior MousePosition to
-- filter the event with.
filterClipB :: Behavior Viewport
            -> Behavior MousePosition
            -> Event a
            -> Event a
filterClipB viewportB mousePosB = whenE isInside
  where insideViewport' vp (MousePosition x y) = insideViewport vp x y
        isInside = insideViewport' <$> viewportB <*> mousePosB
