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

-- | Zoom a camera, keeping the point under the mouse still while zooming.
mouseZoom :: GL.Size -> GL.Position -> GL.GLfloat -> PanZoomCamera -> PanZoomCamera
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


initCamera' :: PanZoomCamera -> CanvasEvents ->
               MomentIO (Behavior Transformation, CanvasEventHandler)
initCamera' cam events =
  do sCam <- subject cam

     bPressedButtons <- recordButtons events (sCam ^. behavior)

     -- Why are we creating new events? We need them to avoid the circular
     -- dependency on canvas events and events passed to event handlers. More
     -- specifically, we want to create event handlers for camera actions, but
     -- we only have the root canvas events available. Therefore, we create
     -- dummy events that we will fire with the event handlers.
     (ePos, fPos) <- newEvent
     let hPos e = reactimate (fPos <$> e) >> return NotAccepted
     eMove <- dragMove events bPressedButtons sCam ePos
     reactimate eMove

     (eScroll, fScroll) <- newEvent
     let hScroll e = reactimate (fScroll <$> e) >> return NotAccepted
     doScroll <- scroll events sCam eScroll
     reactimate doScroll

     let winEventHandler = canvasEventHandler
                           { mousePosEventHandler    = Just hPos
                           , mouseScrollEventHandler = Just hScroll
                           }

     return (cameraTrans <$> sCam ^. behavior, winEventHandler)


recordButtons :: CanvasEvents ->
                 Behavior PanZoomCamera ->
                 MomentIO (Behavior (PressedButtons PanZoomCamera))
recordButtons events bCam =
  do let bCamPos = (,) <$> bCam
                       <*> (events ^. mousePosObservable ^. behavior)
         eTagged = (,) <$> bCamPos <@> (events ^. mouseButtonEvent)
         applyClick :: ((PanZoomCamera, GL.Position), MouseButtonEvent) ->
                       PressedButtons PanZoomCamera ->
                       PressedButtons PanZoomCamera
         applyClick ((s, p), (b, bs)) = recordClick s b bs p
     accumB pressedButtons (applyClick <$> eTagged)

dragMove :: CanvasEvents ->
            Behavior (PressedButtons PanZoomCamera) ->
            Subject PanZoomCamera ->
            Event GL.Position ->
            MomentIO (Event (IO ()))
dragMove events bPressedButtons sCam ePos =
  do let bPressedSize = (,,) <$> bPressedButtons
                             <*> events ^. canvasSizeObservable ^. behavior
                             <*> sCam ^. behavior
         eDoMove = (,) <$> bPressedSize <@> ePos
         doMove :: ((PressedButtons PanZoomCamera, GL.Size, PanZoomCamera), GL.Position) -> IO ()
         doMove ((pbs, size, cs), pos) =
           do let bs = Map.lookup MouseButtonLeft (buttonMap pbs)
              case bs of
                Nothing    -> return ()
                (Just bs') -> (sCam ^. handler) $ mouseDrag size pos bs' cs
     return $ doMove <$> eDoMove

-- | Change the camera state with a mouse drag.
mouseDrag :: GL.Size ->
             GL.Position ->
             (GL.Position, PanZoomCamera) ->
             PanZoomCamera ->
             PanZoomCamera
mouseDrag (GL.Size w h) (GL.Position x y) (GL.Position ox oy, ocs) cs =
  cs { center = c }
  where
    (dx, dy) = (x - ox, y - oy)
    (cw, ch) = (width cs, height cs)
    dxw = realToFrac $ fromIntegral dx * cw / fromIntegral w
    dyw = realToFrac $ fromIntegral dy * ch / fromIntegral h
    c   = center ocs + L.V2 (-dxw) dyw


scroll :: CanvasEvents ->
          Subject PanZoomCamera ->
          Event GL.GLfloat ->
          MomentIO (Event (IO ()))
scroll events sCam eScroll =
  do let bPosSize = (,,) <$> events ^. canvasSizeObservable ^. behavior
                         <*> events ^. mousePosObservable ^. behavior
                         <*> sCam ^. behavior
         eDoScroll = (,) <$> bPosSize <@> eScroll
         doScroll :: ((GL.Size, GL.Position, PanZoomCamera), GL.GLfloat) -> IO ()
         doScroll ((size, pos, cs), ds) = (sCam ^. handler) $ mouseZoom size pos ds cs
     return $ doScroll <$> eDoScroll
