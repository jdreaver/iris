-- | Class definition and utilities for all cameras

module Iris.Camera.Class
       ( Camera (..)
       , PressedButtons
       , pressedButtons
       , recordButtons
       , recordClick
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
