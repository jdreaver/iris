-- | Class definition and utilities for all cameras

module Iris.Camera.Class
       ( Camera (..)
       , PressedButtons
       , pressedButtons
       , recordButtons
       , recordClick
       ) where

import           Control.Lens
import qualified Data.Map.Strict as Map
import qualified Graphics.Rendering.OpenGL as GL

import           Iris.Backends
import           Iris.Mouse
import           Iris.Reactive
import           Iris.Transformation

-- | Type class for all cameras.
class Camera a where
  initCamera :: a -> CanvasEvents -> MomentIO (Behavior Transformation)


-- | Stores currently pressed buttons and the mouse coordinates when they were
-- pressed.
type PressedButtons = Map.Map MouseButton GL.Position

-- | Default constructor for `PressedButtons`
pressedButtons :: PressedButtons
pressedButtons = Map.fromList []


-- | Creates an Event that holds the currently pressed buttons.
recordButtons :: CanvasEvents ->
                 Moment (Event PressedButtons)
recordButtons events = accumE pressedButtons eClickedCam
  where applyClick p (b, bs) = recordClick p b bs
        eClickedCam = applyClick <$> (events ^. mousePosObservable ^. behavior)
                                 <@> (events ^. mouseButtonEvent)


-- | Record when a button is pressed in the `PressedButtons` state.
recordClick :: GL.Position ->
               MouseButton ->
               MouseButtonState ->
               PressedButtons ->
               PressedButtons
recordClick p button Pressed bmap = Map.insert button p bmap
recordClick _ button Released bmap = Map.delete button bmap
