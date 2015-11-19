-- | Class definition and utilities for all cameras

module Iris.Camera.Class
       ( Camera (..)
       , PressedButtons (..)
       , pressedButtons
       , recordClick
       ) where

import qualified Data.Map.Strict as Map
import qualified Graphics.Rendering.OpenGL as GL

import           Iris.Backends
import           Iris.Mouse
import           Iris.Reactive
import           Iris.Transformation

-- | Type class for all cameras.
class Camera a where
  initCamera :: a -> CanvasEvents -> MomentIO (Behavior Transformation, CanvasEventHandler)


-- | Stores currently pressed buttons and the coordinates when they were
-- pressed.
newtype (Camera a) => PressedButtons a = PressedButtons
  { buttonMap :: Map.Map MouseButton (GL.Position, a) }
  deriving (Show)


-- | Default constructor for `PressedButtons`
pressedButtons :: (Camera a) => PressedButtons a
pressedButtons = PressedButtons (Map.fromList [])


-- | If a button is pressed, then return the state when that button was
-- pressed.
-- buttonPressed :: MouseButton -> PressedButtons -> Maybe ButtonPressState
-- buttonPressed b = Map.lookup b . buttonMap


-- | Record when a button is pressed in the `PressedButtons` state.
recordClick :: (Camera a) =>
               a ->
               GL.Position ->
               MouseButton ->
               MouseButtonState ->
               PressedButtons a ->
               PressedButtons a
recordClick c p button Pressed (PressedButtons bmap) =
  if Map.member button bmap
  then PressedButtons bmap
  else PressedButtons $ Map.insert button (p, c) bmap
recordClick _ _ button Released (PressedButtons bmap) =
  PressedButtons $ Map.delete button bmap
