-- | Defines mouse interaction data and functions

module Iris.Mouse
       ( MouseButton (..)
       , MouseButtonState (..)
       , PressedButtons (..)
       , buttonPressed
       , drag
       , pressedButtons
       , recordClick
       ) where

import qualified Data.Map.Strict as Map
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L

import Iris.Camera


-- | Enum for the mouse buttons used for plot interaction. Many backends define
-- more mouse buttons, and users can manually handle them if they want.
data MouseButton = MouseButtonLeft
                 | MouseButtonRight
                 | MouseButtonMiddle
                 deriving (Show, Eq, Ord)


-- | Simple enum for whether a mouse button is pressed or not.
data MouseButtonState = Pressed
                      | Released
                      deriving (Show, Eq)


-- | Stores currently pressed buttons and the coordinates when they were
-- pressed.
newtype PressedButtons = PressedButtons
  { buttonMap :: Map.Map MouseButton ButtonPressState }
  deriving (Show)

-- | Position and stored camera center when a button is pressed. Used for
-- implementing camera movement with the mouse.
type ButtonPressState = (GL.GLint, GL.GLint, CameraCenter)

buttonPressed :: MouseButton -> PressedButtons -> Maybe ButtonPressState
buttonPressed b = Map.lookup b . buttonMap

pressedButtons :: PressedButtons
pressedButtons = PressedButtons (Map.fromList [])

-- | Record when a button is pressed in the `PressedButtons` state.
recordClick :: CameraCenter ->
               MouseButton ->
               MouseButtonState ->
               GL.Position ->
               PressedButtons ->
               PressedButtons
recordClick c button Pressed (GL.Position x y) (PressedButtons bmap) =
  if Map.member button bmap
  then PressedButtons bmap
  else PressedButtons $ Map.insert button (x, y, c) bmap
recordClick _ button Released _ (PressedButtons bmap) =
  PressedButtons $ Map.delete button bmap


-- | Change the camera state with a mouse drag.
drag :: GL.Size ->
        GL.Position ->
        ButtonPressState ->
        CameraState ->
        CameraState
drag (GL.Size w h) (GL.Position x y) (ox, oy, ocs) cs = cs { center = c }
  where
    (dx, dy) = (x - ox, y - oy)
    (cw, ch) = (width cs, height cs)
    dxw = realToFrac $ fromIntegral dx * cw / fromIntegral w
    dyw = realToFrac $ fromIntegral dy * ch / fromIntegral h
    c   = ocs + L.V2 (-dxw) dyw
