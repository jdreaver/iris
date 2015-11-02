{-# LANGUAGE TypeFamilies #-}

-- | Handle camera interaction in a scene

module Iris.Camera
       ( CameraState (..)
       , CameraCenter
       , PressedButtons (..)
       , buttonPressed
       , mouseDrag
       , mouseZoom
       , pressedButtons
       , recordClick
       ) where

import qualified Data.Map.Strict as Map
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L

import           Iris.Mouse

-- | Vector holding the position of the center of a 2D camera.
type CameraCenter = L.V2 GL.GLfloat

-- | Container for 2D plot camera info. This is usually put in a TVar,
-- written to by interaction callbacks, and read from the OpenGL draw function
-- to create the transformation matrix.
data CameraState = CameraState
  { center :: CameraCenter
  , width  :: GL.GLfloat
  , height :: GL.GLfloat
  } deriving (Show)


-- | Stores currently pressed buttons and the coordinates when they were
-- pressed.
newtype PressedButtons = PressedButtons
  { buttonMap :: Map.Map MouseButton ButtonPressState }
  deriving (Show)

-- | Default constructor for `PressedButtons`
pressedButtons :: PressedButtons
pressedButtons = PressedButtons (Map.fromList [])


-- | Position and stored camera center when a button is pressed. Used for
-- implementing camera movement with the mouse.
type ButtonPressState = (GL.Position, CameraCenter)

-- | If a button is pressed, then return the state when that button was
-- pressed.
buttonPressed :: MouseButton -> PressedButtons -> Maybe ButtonPressState
buttonPressed b = Map.lookup b . buttonMap

-- | Record when a button is pressed in the `PressedButtons` state.
recordClick :: CameraCenter ->
               MouseButton ->
               MouseButtonState ->
               GL.Position ->
               PressedButtons ->
               PressedButtons
recordClick c button Pressed p (PressedButtons bmap) =
  if Map.member button bmap
  then PressedButtons bmap
  else PressedButtons $ Map.insert button (p, c) bmap
recordClick _ button Released _ (PressedButtons bmap) =
  PressedButtons $ Map.delete button bmap


-- | Change the camera state with a mouse drag.
mouseDrag :: GL.Size ->
             GL.Position ->
             ButtonPressState ->
             CameraState ->
             CameraState
mouseDrag (GL.Size w h) (GL.Position x y) (GL.Position ox oy, ocs) cs =
  cs { center = c }
  where
    (dx, dy) = (x - ox, y - oy)
    (cw, ch) = (width cs, height cs)
    dxw = realToFrac $ fromIntegral dx * cw / fromIntegral w
    dyw = realToFrac $ fromIntegral dy * ch / fromIntegral h
    c   = ocs + L.V2 (-dxw) dyw

-- | Zoom a camera, keeping the point under the mouse still while zooming.
mouseZoom :: GL.Size -> GL.Position -> GL.GLfloat -> CameraState -> CameraState
mouseZoom s p z cs =
  cs { center = c' , width = w' , height = h' }
  where
    f = realToFrac $ 1 - 0.1 * z  -- Zoom factor
    w = width cs
    h = height cs
    w' = w * f
    h' = h * f

    -- Translate center
    (x', y') = mapToWorld s p cs
    x = L.V2 (realToFrac x') (realToFrac y')
    dx = x - c
    dx' = dx * realToFrac f
    c = center cs
    c' = x - dx'

-- | Map from pixel window coordinates to world coordinates.
mapToWorld :: GL.Size -> GL.Position -> CameraState -> (GL.GLfloat, GL.GLfloat)
mapToWorld (GL.Size w h) (GL.Position xp yp) (CameraState (L.V2 cx cy) cw ch) = (x, y)
  where (w', h')   = (fromIntegral w, fromIntegral h)
        (cxp, cyp) = (w' / 2, h' / 2)  -- Camera center in pixels
        (xp', yp') = (fromIntegral xp, fromIntegral yp)
        (dxp, dyp) = (xp' - cxp, yp' - cyp)
        (cw', ch') = (cw, ch)
        (cx', cy') = (cx, cy)
        (dx, dy)   = (dxp * cw' / w', dyp * ch' / h' * (-1))
        (x, y)     = (cx' + dx, cy' + dy)
