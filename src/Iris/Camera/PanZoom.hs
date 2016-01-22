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


-- | The first part of the tuple is a map of the mouse state and position when
-- the mouse is pressed. The second part is the actual camera state.
type PanZoomState = (Map.Map MouseButton (PanZoomCamera, MousePosition), PanZoomCamera)

panZoomTransB :: PanZoomCamera
              -> CanvasEvents
              -> MomentIO (Behavior Transformation)
panZoomTransB cam events =
  do let mousePosB    = events ^. mousePosObservable ^. behavior
         mouseButtonE = events ^. mouseButtonEvent
     ePressedButtons <- liftMoment $ recordButtons mousePosB mouseButtonE

     let eClick = panZoomClickEvent ePressedButtons
         eMovedCam = panZoomDragEvent events
         eScrolledCam = panZoomScrollEvent events

     bCamState <- accumB (Map.fromList [], cam) $ unions [ eClick, eMovedCam, eScrolledCam ]

     return $ panZoomTrans <$> (snd <$> bCamState)


-- | Tags a PressedButtons map with the current camera state.
panZoomClickEvent :: Event PressedButtons
                  -> Event (PanZoomState -> PanZoomState)
panZoomClickEvent ePressedButtons = f <$> ePressedButtons
  where f :: PressedButtons -> PanZoomState -> PanZoomState
        f pb (_, cs) = (fmap ((,) cs) pb, cs)


panZoomDragEvent :: CanvasEvents
                 -> Event (PanZoomState -> PanZoomState)
panZoomDragEvent events =
  doMove <$> events ^. canvasSizeObservable ^. behavior
         <@> events ^. mousePosObservable ^. event
  where doMove size pos (cm, cs) =
          maybe (cm, cs)
          (\(cso, opos) -> (cm, panZoomMouseDrag size opos cso pos cs))
          (Map.lookup (dragButton cs) cm)

-- | Change the camera state with a mouse drag.
panZoomMouseDrag :: CanvasSize
                 -> MousePosition -- ^ Original mouse position
                 -> PanZoomCamera -- ^ Original state
                 -> MousePosition -- ^ Current mouse position
                 -> PanZoomCamera -- ^ Current state
                 -> PanZoomCamera -- ^ New state
panZoomMouseDrag (CanvasSize w h) (MousePosition ox oy) ocs (MousePosition x y) cs =
  cs { center = center ocs + L.V2 (-dxw) dyw }
  where
    dxw = panZoomAxisDrag ox x (width cs) w
    dyw = panZoomAxisDrag oy y (height cs) h

-- | Computes the difference in the camera center coordinate needed to move a
-- corresponding mouse coordinate while keeping the point under the mouse still
-- under the mouse.
panZoomAxisDrag :: GL.GLint   -- ^ Original mouse coordinate
                -> GL.GLint   -- ^ Current mouse coordinate
                -> GL.GLfloat -- ^ Camera width
                -> GL.GLint   -- ^ Canvas width
                -> GL.GLfloat -- ^ Change in camera width
panZoomAxisDrag ox x cw w = realToFrac $ fromIntegral (x - ox) * cw / fromIntegral w


panZoomScrollEvent :: CanvasEvents
                   -> Event (PanZoomState -> PanZoomState)
panZoomScrollEvent events =
  doZoom <$> events ^. canvasSizeObservable ^. behavior
         <*> events ^. mousePosObservable ^. behavior
         <@> events ^. mouseScrollEvent
  where doZoom s p z (cm, cs) = (cm, panZoomMouseZoom s p cs z)


-- | Zoom a camera, keeping the point under the mouse still while zooming.
panZoomMouseZoom :: CanvasSize
                 -> MousePosition     -- ^ Current mouse position
                 -> PanZoomCamera     -- ^ Current state
                 -> MouseScrollAmount -- ^ How much mouse wheel was turned
                 -> PanZoomCamera     -- ^ New camera state
panZoomMouseZoom (CanvasSize w h)
                 (MousePosition xp yp)
                 (PanZoomCamera (L.V2 cx cy) cw ch b)
                 (MouseScrollAmount z) =
  PanZoomCamera (L.V2 cx' cy') (cw * factor) (ch * factor) b
  where
    factor = realToFrac $ 1 - 0.1 * z  -- Zoom factor

    -- Translate center
    x   = mapAxisPixelToWorld w xp cw cx
    y   = mapAxisPixelToWorld h (h - yp) ch cy
    cx' = x - (x - cx) * factor
    cy' = y - (y - cy) * factor

-- | Map from a pixel coordinate (like a mouse coordinate) to world coordinates
-- along an axis.
mapAxisPixelToWorld :: GL.GLint   -- ^ Canvas width
                    -> GL.GLint   -- ^ Mouse pixel position
                    -> GL.GLfloat -- ^ Camera width in world coordinates
                    -> GL.GLfloat -- ^ Camera center in world coordinates
                    -> GL.GLfloat -- ^ Mouse in world coordinates
mapAxisPixelToWorld w xp cw cx = cx + dx
  where cxp = fromIntegral w / 2    -- Camera center in pixels
        dxp = fromIntegral xp - cxp -- Diff between cam center and mouse in pixels
        dx  = dxp * cw / fromIntegral w -- Diff in world coordinates
