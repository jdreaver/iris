-- | Class definition and utilities for all cameras

module Iris.Camera.Class
       ( Camera (..)
       , PressedButtons
       , pressedButtons
       , recordButtons
       , recordClick
       , mouseDragEvent
       , MouseDrag (..)
       ) where

import qualified Data.Map.Strict as Map

import           Iris.Backends
import           Iris.Mouse
import           Iris.Reactive
import           Iris.Transformation

-- | Type class for all cameras.
class Camera a where
  cameraTrans         :: a -> Transformation
  cameraTransBehavior :: a -> CanvasEvents -> MomentIO (Behavior Transformation)


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


-- | Constructs an event that fires whenever the mouse is dragged. A mouse drag
-- begins when any mouse button is pressed, and the mouse is moved. While the
-- drag is happening, any other button can be pressed too, but this doesn't
-- "restart" the drag.
mouseDragEvent :: Event MouseButtonEvent
               -> Observable MousePosition
               -> MomentIO (Event MouseDrag)
mouseDragEvent buttonE (Observable posB posE) =
  do pressedButtonsE <- liftMoment $ recordButtons posB buttonE
     pressedButtonsB <- stepper Map.empty pressedButtonsE
     let updateDragE = updateMouseDrag <$> pressedButtonsB <@> posE
     eDragMaybe <- accumE Nothing updateDragE
     return $ filterJust eDragMaybe

-- | Container to hold information about a mouse drag event.
data MouseDrag = MouseDrag
  { mouseDragOriginPos  :: !MousePosition
  , mouseDragCurrentPos :: !MousePosition
  , mouseDragButtons    :: ![MouseButton]
  } deriving (Show, Ord, Eq)


updateMouseDrag :: PressedButtons
                -> MousePosition
                -> Maybe MouseDrag -- ^ Current drag state; Nothing = no drag
                -> Maybe MouseDrag
updateMouseDrag pbs pos maybeDrag = if null buttons then Nothing else newDrag
  where buttons = map fst $ Map.toList pbs
        origin  = maybe pos mouseDragOriginPos maybeDrag
        newDrag = Just $ MouseDrag origin pos buttons
