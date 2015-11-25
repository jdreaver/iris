-- | Class definition and utilities for all cameras

module Iris.Camera.Class
       ( Camera (..)
       , PressedButtons (..)
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
  initCamera :: a -> CanvasEvents -> MomentIO (Behavior Transformation, CanvasEventHandler)


-- | Stores currently pressed buttons and the mouse coordinates when they were
-- pressed.
newtype PressedButtons = PressedButtons
  { buttonMap :: Map.Map MouseButton GL.Position }
  deriving (Show)


-- | Default constructor for `PressedButtons`
pressedButtons :: PressedButtons
pressedButtons = PressedButtons (Map.fromList [])


-- | Creates a Behavior that holds the currently pressed buttons.
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
recordClick p button Pressed (PressedButtons bmap) =
  if Map.member button bmap
  then PressedButtons bmap
  else PressedButtons $ Map.insert button p bmap
recordClick _ button Released (PressedButtons bmap) =
  PressedButtons $ Map.delete button bmap
