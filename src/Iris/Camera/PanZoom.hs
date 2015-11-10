-- | Definition of a pan-zoom camera. This is a great camera for 2D plots.

module Iris.Camera.PanZoom
       ( PanZoomCamera (..)
       , panZoomCamera
       , mouseDrag
       , mouseZoom
       , mouseNetwork
       ) where

import           Control.Concurrent.STM
import           Control.Lens
import           Data.List (foldl1')
import qualified Data.Map.Strict as Map
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L
import           Reactive.Banana
import           Reactive.Banana.Frameworks

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
  cameraTrans = cameraTrans'

-- | Default PanZoomCamera instance.
panZoomCamera :: PanZoomCamera
panZoomCamera = PanZoomCamera (L.V2 0 0) 1 1 MouseButtonLeft


-- | Creates a transformation that centers from the camera center to (0, 0),
-- and scales widths and heights from the camera width/height to [-1, 1].
cameraTrans' :: PanZoomCamera -> Transformation
cameraTrans' (PanZoomCamera (L.V2 cx cy) w h _) =
  foldl1' Iris.Transformation.apply [scale', trans, identity]
  where trans  = translation (L.V3 (-cx) (-cy) 0)
        scale' = scale (L.V3 (2/w) (2/h) 1)

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

-- | Map from pixel window coordinates to world coordinates.
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


mouseNetwork :: (Window a) => TVar PanZoomCamera -> a -> MomentIO ()
mouseNetwork camTVar win =
  do events <- W.makeEvents win

     -- Do we really need to create a new event? We need a recursive definition
     -- of camera state.
     sCam <- tVarSubject camTVar

     bPressedButtons <- recordButtons events (sCam ^. behavior)

     eMove <- dragMove events bPressedButtons sCam
     reactimate eMove

     doScroll <- scroll events sCam
     reactimate doScroll


recordButtons :: WindowEvents ->
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

dragMove :: WindowEvents ->
            Behavior (PressedButtons PanZoomCamera) ->
            Subject PanZoomCamera ->
            MomentIO (Event (IO ()))
dragMove events bPressedButtons sCam =
  do let bPressedSize = (,,) <$> bPressedButtons
                             <*> events ^. windowSizeObservable ^. behavior
                             <*> sCam ^. behavior
         eDoMove = (,) <$> bPressedSize
                       <@> events ^. mousePosObservable ^. event
         doMove :: ((PressedButtons PanZoomCamera, GL.Size, PanZoomCamera), GL.Position) -> IO ()
         doMove ((pbs, size, cs), pos) =
           do let bs = Map.lookup MouseButtonLeft (buttonMap pbs)
              case bs of
                Nothing    -> return ()
                (Just bs') -> (sCam ^. handler) $ mouseDrag size pos bs' cs
     return $ doMove <$> eDoMove

scroll :: WindowEvents ->
          Subject PanZoomCamera ->
          MomentIO (Event (IO ()))
scroll events sCam =
  do let bPosSize = (,,) <$> events ^. windowSizeObservable ^. behavior
                         <*> events ^. mousePosObservable ^. behavior
                         <*> sCam ^. behavior
         eDoScroll = (,) <$> bPosSize
                         <@> events ^. mouseScrollEvent
         doScroll :: ((GL.Size, GL.Position, PanZoomCamera), GL.GLfloat) -> IO ()
         doScroll ((size, pos, cs), ds) = (sCam ^. handler) $ mouseZoom size pos ds cs
     return $ doScroll <$> eDoScroll
