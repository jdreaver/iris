-- | Definition of a pan-zoom camera. This is a great camera for 2D plots.

module Iris.Camera.PanZoom
       ( PanZoomCamera (..)
       , panZoomCamera
       , panZoomTrans
       ) where

import           Control.Lens
import           Data.List (foldl1')
import qualified Data.Map.Strict as Map
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L

import           Iris.Backends as W
import           Iris.Camera.Class
import           Iris.Mouse
import           Iris.Reactive
import           Iris.Transformation



-- | Camera that pans along the XY plane.
data PanZoomCamera = PanZoomCamera
  { center :: L.V2 GL.GLfloat
  , width  :: GL.GLfloat
  , height :: GL.GLfloat
  , dragButton :: MouseButton
  } deriving (Show)

instance Camera PanZoomCamera where
  initCamera = initCamera'

-- | Default PanZoomCamera instance.
panZoomCamera :: PanZoomCamera
panZoomCamera = PanZoomCamera (L.V2 0 0) 1 1 MouseButtonLeft


-- | Creates a transformation that centers from the camera center to (0, 0),
-- and scales widths and heights from the camera width/height to [-1, 1].
panZoomTrans :: PanZoomCamera -> Transformation
panZoomTrans (PanZoomCamera (L.V2 cx cy) w h _) =
  foldl1' Iris.Transformation.apply [scale', trans, identity]
  where trans  = translation (L.V3 (-cx) (-cy) 0)
        scale' = scale (L.V3 (2/w) (2/h) 1)


-- | The first part of the tuple is a map of the mouse state and position when
-- the mouse is pressed. The second part is the actual camera state.
type PanZoomState = (Map.Map MouseButton (PanZoomCamera, MousePosition), PanZoomCamera)

initCamera' :: PanZoomCamera -> CanvasEvents ->
               MomentIO (Behavior Transformation)
initCamera' cam events =
  do ePressedButtons <- liftMoment $ recordButtons events

     let eClick = panZoomClickEvent ePressedButtons
         eMovedCam = panZoomDragEvent events
         eScrolledCam = panZoomScrollEvent events

     bCamState <- accumB (Map.fromList [], cam) $ unions [ eClick, eMovedCam, eScrolledCam ]

     return $ panZoomTrans <$> (snd <$> bCamState)


-- | Tags a PressedButtons map with the current camera state.
panZoomClickEvent :: Event PressedButtons ->
                     Event (PanZoomState -> PanZoomState)
panZoomClickEvent ePressedButtons = f <$> ePressedButtons
  where f :: PressedButtons -> PanZoomState -> PanZoomState
        f pb (_, cs) = (fmap ((,) cs) pb, cs)


panZoomDragEvent :: CanvasEvents ->
                    Event (PanZoomState -> PanZoomState)
panZoomDragEvent events =
  doMove <$> events ^. canvasSizeObservable ^. behavior
         <@> events ^. mousePosObservable ^. event
  where doMove size pos (cm, cs) =
          maybe (cm, cs)
          (\(cso, opos) -> (cm, panZoomMouseDrag size opos cso pos cs))
          (Map.lookup (dragButton cs) cm)

-- | Change the camera state with a mouse drag.
panZoomMouseDrag :: CanvasSize ->
                    MousePosition -> -- ^ Original mouse position
                    PanZoomCamera -> -- ^ Original state
                    MousePosition -> -- ^ Current mouse position
                    PanZoomCamera -> -- ^ Current state
                    PanZoomCamera    -- ^ New state
panZoomMouseDrag (CanvasSize w h) (MousePosition ox oy) ocs (MousePosition x y) cs =
  cs { center = c }
  where
    (dx, dy) = (x - ox, y - oy)
    (cw, ch) = (width cs, height cs)
    dxw = realToFrac $ fromIntegral dx * cw / fromIntegral w
    dyw = realToFrac $ fromIntegral dy * ch / fromIntegral h
    c   = center ocs + L.V2 (-dxw) dyw


panZoomScrollEvent :: CanvasEvents ->
                      Event (PanZoomState -> PanZoomState)
panZoomScrollEvent events =
  doZoom <$> events ^. canvasSizeObservable ^. behavior
         <*> events ^. mousePosObservable ^. behavior
         <@> events ^. mouseScrollEvent
  where doZoom s p z (cm, cs) = (cm, panZoomMouseZoom s p cs z)


-- | Zoom a camera, keeping the point under the mouse still while zooming.
panZoomMouseZoom :: CanvasSize ->
                    MousePosition ->     -- ^ Current mouse position
                    PanZoomCamera ->     -- ^ Current state
                    MouseScrollAmount -> -- ^ How much mouse wheel was turned
                    PanZoomCamera        -- ^ New camera state
panZoomMouseZoom s p cs (MouseScrollAmount z) =
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


-- | Map from pixel canvas coordinates to world coordinates.
mapToWorld :: CanvasSize -> MousePosition -> PanZoomCamera -> (GL.GLfloat, GL.GLfloat)
mapToWorld (CanvasSize w h) (MousePosition xp yp) cam = (x, y)
  where (L.V2 cx cy) = center cam
        (w', h')   = (fromIntegral w, fromIntegral h)
        (cxp, cyp) = (w' / 2, h' / 2)  -- Camera center in pixels
        (xp', yp') = (fromIntegral xp, fromIntegral yp)
        (dxp, dyp) = (xp' - cxp, yp' - cyp)
        (cw', ch') = (width cam, height cam)
        (cx', cy') = (cx, cy)
        (dx, dy)   = (dxp * cw' / w', dyp * ch' / h' * (-1))
        (x, y)     = (cx' + dx, cy' + dy)
