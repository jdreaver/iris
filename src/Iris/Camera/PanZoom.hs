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
  { panZoomCenter     :: L.V2 GL.GLfloat
  , panZoomWidth      :: GL.GLfloat
  , panZoomHeight     :: GL.GLfloat
  , panZoomDragButton :: MouseButton
  } deriving (Show)

instance Camera PanZoomCamera where
  cameraTrans    = panZoomTrans
  camFromEventsB = panZoomFromEvents

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

panZoomFromEvents :: PanZoomCamera
                  -> CanvasEvents
                  -> MomentIO (Behavior Transformation)
panZoomFromEvents cam (CanvasEvents mousePosO mouseButtonE mouseScrollE viewportO _ _) =
  panZoomTransB cam mousePosO mouseButtonE viewportB mouseScrollE
  where (Observable viewportB _) = viewportO


panZoomTransB :: PanZoomCamera
              -> Observable MousePosition
              -> Event MouseButtonEvent
              -> Behavior Viewport
              -> Event MouseScrollAmount
              -> MomentIO (Behavior Transformation)
panZoomTransB cam mousePosO mouseButtonE viewportB mouseScrollE =
  do (_, dragE) <- liftMoment $ mouseDragEvents mouseButtonE mousePosO
     bCamState <- accumB (cam, cam) $
       unions [ panZoomClick <$> mouseButtonE
              , panZoomDrag <$> viewportB <@> dragE
              , panZoomZoom <$> viewportB
                            <*> observableBehavior mousePosO
                            <@> mouseScrollE
              ]
     return $ panZoomTrans <$> (snd <$> bCamState)

-- | When we begin a drag, update the PanZoomState if the drag button is the
-- same as the camera's drag button.
panZoomClick :: MouseButtonEvent -> PanZoomState -> PanZoomState
panZoomClick (bn, Pressed) cs@(_, cam)
  | bn == panZoomDragButton cam = (cam, cam)
  | otherwise                   = cs
panZoomClick _ cs = cs

panZoomDrag :: Viewport -> MouseDrag -> PanZoomState -> PanZoomState
panZoomDrag cs (MouseDrag opos pos bn) (ocam, cam) = (ocam, cam')
  where cam' = if bn == [panZoomDragButton cam] then newCam else cam
        newCam = panZoomMouseDrag cs opos ocam pos cam


-- | Change the camera state with a mouse drag.
panZoomMouseDrag :: Viewport
                 -> MousePosition -- ^ Original mouse position
                 -> PanZoomCamera -- ^ Original state
                 -> MousePosition -- ^ Current mouse position
                 -> PanZoomCamera -- ^ Current state
                 -> PanZoomCamera -- ^ New state
panZoomMouseDrag (Viewport _ (GL.Size wp hp))
                 (MousePosition oxp oyp)
                 ocs
                 (MousePosition xp yp)
                 cs =
  cs { panZoomCenter = panZoomCenter ocs + L.V2 (-dxw) dyw }
  where
    dxw = panZoomAxisDrag oxp xp (panZoomWidth cs) wp
    dyw = panZoomAxisDrag oyp yp (panZoomHeight cs) hp

-- | Computes the difference in the camera center coordinate needed to move a
-- corresponding mouse coordinate while keeping the point under the mouse still
-- under the mouse.
panZoomAxisDrag :: GL.GLint   -- ^ Original mouse coordinate
                -> GL.GLint   -- ^ Current mouse coordinate
                -> GL.GLfloat -- ^ Camera width
                -> GL.GLint   -- ^ Canvas width
                -> GL.GLfloat -- ^ Change in camera width
panZoomAxisDrag oxp xp cw w = realToFrac $ fromIntegral (xp - oxp) * cw / fromIntegral w


panZoomZoom :: Viewport
            -> MousePosition     -- ^ Current mouse position
            -> MouseScrollAmount -- ^ How much mouse wheel was turned
            -> PanZoomState      -- ^ Current state
            -> PanZoomState      -- ^ New camera state
panZoomZoom cans mp msa (cso, cs) = (cso, panZoomMouseZoom cans mp msa cs)

-- | Zoom a camera, keeping the point under the mouse still while zooming.
panZoomMouseZoom :: Viewport
                 -> MousePosition     -- ^ Current mouse position
                 -> MouseScrollAmount -- ^ How much mouse wheel was turned
                 -> PanZoomCamera     -- ^ Current state
                 -> PanZoomCamera     -- ^ New camera state
panZoomMouseZoom (Viewport _ (GL.Size wp hp))
                 (MousePosition mxp myp)
                 (MouseScrollAmount z)
                 (PanZoomCamera (L.V2 cx cy) cw ch b) =
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
