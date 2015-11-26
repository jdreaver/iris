-- | Definition of a pan-zoom camera. This is a great camera for 2D plots.

module Iris.Camera.PanZoom
       ( PanZoomCamera (..)
       , panZoomCamera
       ) where

import           Control.Lens
import           Data.List (foldl1')
import qualified Data.Map.Strict as Map
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L

import           Iris.Backends as W
import           Iris.Camera.Class
import           Iris.Events
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
cameraTrans :: PanZoomCamera -> Transformation
cameraTrans (PanZoomCamera (L.V2 cx cy) w h _) =
  foldl1' Iris.Transformation.apply [scale', trans, identity]
  where trans  = translation (L.V3 (-cx) (-cy) 0)
        scale' = scale (L.V3 (2/w) (2/h) 1)


-- | The first part of the tuple is a map of the mouse state and position when
-- the mouse is pressed. The second part is the actual camera state.
type PanZoomState = (Map.Map MouseButton (PanZoomCamera, GL.Position), PanZoomCamera)

initCamera' :: PanZoomCamera -> CanvasEvents ->
               MomentIO (Behavior Transformation, CanvasEventHandler)
initCamera' cam events =
  do -- Why are we creating new events? We need them to avoid the circular
     -- dependency on canvas events and events passed to event handlers. More
     -- specifically, we want to create event handlers for camera actions, but
     -- we only have the root canvas events available. Therefore, we create
     -- dummy events that we will fire with the event handlers.
     (ePos, hPos) <- eventHandler NotAccepted
     (eScroll, hScroll) <- eventHandler NotAccepted
     ePressedButtons <- liftMoment $ recordButtons events

     let eClick = clickEvent ePressedButtons
         eMovedCam = dragEvent events ePos
         eScrolledCam = scrollEvent events eScroll

         winEventHandler = canvasEventHandler
                           { mousePosEventHandler    = Just hPos
                           , mouseScrollEventHandler = Just hScroll
                           }

     bCamState <- accumB (Map.fromList [], cam) $ unions [ eClick, eMovedCam, eScrolledCam ]

     return (cameraTrans <$> (snd <$> bCamState), winEventHandler)


-- | Tags a PressedButtons map with the current camera state.
clickEvent :: Event PressedButtons ->
              Event (PanZoomState -> PanZoomState)
clickEvent ePressedButtons = f <$> ePressedButtons
  where f :: PressedButtons -> PanZoomState -> PanZoomState
        f pb (_, cs) = (fmap ((,) cs) pb, cs)


dragEvent :: CanvasEvents ->
             Event GL.Position ->
             Event (PanZoomState -> PanZoomState)
dragEvent events ePos =
  doMove <$> events ^. canvasSizeObservable ^. behavior
         <@> ePos
  where doMove size pos (cm, cs) =
          maybe (cm, cs)
          (\(c1, opos) -> (cm, mouseDrag size pos opos c1 cs))
          (Map.lookup (dragButton cs) cm)

-- | Change the camera state with a mouse drag.
mouseDrag :: GL.Size ->
             GL.Position ->
             GL.Position ->
             PanZoomCamera ->
             PanZoomCamera ->
             PanZoomCamera
mouseDrag (GL.Size w h) (GL.Position x y) (GL.Position ox oy) ocs cs =
  cs { center = c }
  where
    (dx, dy) = (x - ox, y - oy)
    (cw, ch) = (width cs, height cs)
    dxw = realToFrac $ fromIntegral dx * cw / fromIntegral w
    dyw = realToFrac $ fromIntegral dy * ch / fromIntegral h
    c   = center ocs + L.V2 (-dxw) dyw


scrollEvent :: CanvasEvents ->
               Event GL.GLfloat ->
               Event (PanZoomState -> PanZoomState)
scrollEvent events eScroll =
  doZoom <$> events ^. canvasSizeObservable ^. behavior
         <*> events ^. mousePosObservable ^. behavior
         <@> eScroll
  where doZoom s p z (cm, cs) = (cm, mouseZoom s p cs z)


-- | Zoom a camera, keeping the point under the mouse still while zooming.
mouseZoom :: GL.Size -> GL.Position -> PanZoomCamera -> GL.GLfloat -> PanZoomCamera
mouseZoom s p cs z =
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
mapToWorld :: GL.Size -> GL.Position -> PanZoomCamera -> (GL.GLfloat, GL.GLfloat)
mapToWorld (GL.Size w h) (GL.Position xp yp) cam = (x, y)
  where (L.V2 cx cy) = center cam
        (w', h')   = (fromIntegral w, fromIntegral h)
        (cxp, cyp) = (w' / 2, h' / 2)  -- Camera center in pixels
        (xp', yp') = (fromIntegral xp, fromIntegral yp)
        (dxp, dyp) = (xp' - cxp, yp' - cyp)
        (cw', ch') = (width cam, height cam)
        (cx', cy') = (cx, cy)
        (dx, dy)   = (dxp * cw' / w', dyp * ch' / h' * (-1))
        (x, y)     = (cx' + dx, cy' + dy)
