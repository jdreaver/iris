-- | Definition of a pan-zoom camera. This is a great camera for 2D plots.

module Iris.Camera.PanZoom
       ( PanZoomCamera (..)
       , panZoomCamera
       ) where

import           Data.List (foldl1')
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L

import           Iris.Backends as W
import           Iris.Camera.Class
import           Iris.Mouse
import           Iris.Reactive
import           Iris.Transformation


-- | Camera that pans along the XY plane.
data PanZoomCamera = PanZoomCamera
  { center     :: L.V2 GL.GLfloat
  , width      :: GL.GLfloat
  , height     :: GL.GLfloat
  , dragButton :: MouseButton
  } deriving (Show)

instance Camera PanZoomCamera where
  cameraTrans         = panZoomTrans
  cameraTransBehavior = panZoomTransB

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


-- | The first part of the tuple is the camera state at the beginning of a
-- drag. The second part is the actual camera state.
type PanZoomState = (PanZoomCamera, PanZoomCamera)

panZoomTransB :: PanZoomCamera
              -> CanvasEvents
              -> MomentIO (Behavior Transformation)
panZoomTransB cam events@(CanvasEvents mousePosO mouseButtonE _ canvasSizeO _ _) =
  do (_, dragE) <- liftMoment $ mouseDragEvents mouseButtonE mousePosO
     bCamState <- accumB (cam, cam) $
       unions [ click <$> mouseButtonE
              , drag <$> observableBehavior canvasSizeO <@> dragE
              , panZoomScrollEvent events
              ]
     return $ panZoomTrans <$> (snd <$> bCamState)

-- | When we begin a drag, update the PanZoomState if the drag button is the
-- same as the camera's drag button.
click :: MouseButtonEvent -> PanZoomState -> PanZoomState
click (bn, Pressed) cs@(_, cam)
  | bn == dragButton cam = (cam, cam)
  | otherwise            = cs
click _ cs = cs

drag :: CanvasSize -> MouseDrag -> PanZoomState -> PanZoomState
drag cs (MouseDrag opos pos bn) (ocam, cam) = (ocam, cam')
  where cam' = if bn == [dragButton cam] then newCam else cam
        newCam = panZoomMouseDrag cs opos ocam pos cam


-- | Change the camera state with a mouse drag.
panZoomMouseDrag :: CanvasSize
                 -> MousePosition -- ^ Original mouse position
                 -> PanZoomCamera -- ^ Original state
                 -> MousePosition -- ^ Current mouse position
                 -> PanZoomCamera -- ^ Current state
                 -> PanZoomCamera -- ^ New state
panZoomMouseDrag (CanvasSize wp hp) (MousePosition oxp oyp) ocs (MousePosition xp yp) cs =
  cs { center = center ocs + L.V2 (-dxw) dyw }
  where
    dxw = panZoomAxisDrag oxp xp (width cs) wp
    dyw = panZoomAxisDrag oyp yp (height cs) hp

-- | Computes the difference in the camera center coordinate needed to move a
-- corresponding mouse coordinate while keeping the point under the mouse still
-- under the mouse.
panZoomAxisDrag :: GL.GLint   -- ^ Original mouse coordinate
                -> GL.GLint   -- ^ Current mouse coordinate
                -> GL.GLfloat -- ^ Camera width
                -> GL.GLint   -- ^ Canvas width
                -> GL.GLfloat -- ^ Change in camera width
panZoomAxisDrag oxp xp cw w = realToFrac $ fromIntegral (xp - oxp) * cw / fromIntegral w


panZoomScrollEvent :: CanvasEvents
                   -> Event (PanZoomState -> PanZoomState)
panZoomScrollEvent events =
  doZoom <$> observableBehavior (canvasSizeObservable events)
         <*> observableBehavior (mousePosObservable events)
         <@> mouseScrollEvent events
  where doZoom s p z (cm, cs) = (cm, panZoomMouseZoom s p cs z)


-- | Zoom a camera, keeping the point under the mouse still while zooming.
panZoomMouseZoom :: CanvasSize
                 -> MousePosition     -- ^ Current mouse position
                 -> PanZoomCamera     -- ^ Current state
                 -> MouseScrollAmount -- ^ How much mouse wheel was turned
                 -> PanZoomCamera     -- ^ New camera state
panZoomMouseZoom (CanvasSize wp hp)
                 (MousePosition mxp myp)
                 (PanZoomCamera (L.V2 cx cy) cw ch b)
                 (MouseScrollAmount z) =
  PanZoomCamera (L.V2 cx' cy') (cw * factor) (ch * factor) b
  where
    factor = realToFrac $ 1 - 0.1 * z  -- Zoom factor

    -- Translate center
    mx  = mapAxisPixelToWorld wp mxp cw cx
    my  = mapAxisPixelToWorld hp (hp - myp) ch cy
    cx' = (cx - mx) * factor + mx
    cy' = (cy - my) * factor + my

-- | Map from a pixel coordinate (like a mouse coordinate) to world coordinates
-- along an axis.
mapAxisPixelToWorld :: GL.GLint   -- ^ Canvas width
                    -> GL.GLint   -- ^ Mouse pixel position
                    -> GL.GLfloat -- ^ Camera width in world coordinates
                    -> GL.GLfloat -- ^ Camera center in world coordinates
                    -> GL.GLfloat -- ^ Mouse in world coordinates
mapAxisPixelToWorld w mxp cw cx = cx + dx
  where cxp = fromIntegral w / 2        -- Camera center in pixels
        dxp = fromIntegral mxp - cxp    -- Diff between cam center and mouse in pixels
        dx  = dxp * cw / fromIntegral w -- Diff in world coordinates
