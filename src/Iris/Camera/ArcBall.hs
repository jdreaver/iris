-- | Definition of an arcball camera for use in 3D scenes

module Iris.Camera.ArcBall
       ( ArcBallCamera (..)
       , arcBallCamera
       , arcBallTransB
       ) where

import           Data.Fixed (mod')
import           Data.List (foldl1')
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L

import           Iris.Backends
import           Iris.Camera.Class
import           Iris.Mouse
import           Iris.OpenGL (Viewport (..))
import           Iris.Reactive
import           Iris.Transformation

-- | Camera that rotates about a central point
data ArcBallCamera = ArcBallCamera
  { arcBallCenter     :: L.V3 GL.GLfloat
  , arcBallWidth      :: GL.GLfloat
  , arcBallAzimuth    :: GL.GLfloat -- ^ Azimuth in radians
  , arcBallElevation  :: GL.GLfloat -- ^ Elevation in radians
  , arcBallDragButton :: MouseButton
  }

instance Camera ArcBallCamera where
  cameraTrans    = arcBallTrans
  camFromEventsB = arcBallFromCanvasEvents

arcBallCamera :: ArcBallCamera
arcBallCamera = ArcBallCamera (L.V3 0 0 0) 2 0 0 MouseButtonLeft

arcBallTrans :: ArcBallCamera -> Transformation
arcBallTrans (ArcBallCamera (L.V3 cx cy cz) w a e _) =
  foldl1' Iris.Transformation.apply [scale', rotElev, rotAzim, trans, identity]
  where trans    = translation (L.V3 (-cx) (-cy) (-cz))
        rotAzim  = rotateZ a
        rotElev  = rotateX ((pi / 2) - e)
        scale'   = scale (L.V3 (2/w) (2/w) (2/1000))

arcBallFromCanvasEvents :: ArcBallCamera
                        -> CanvasEvents
                        -> MomentIO (Behavior Transformation)
arcBallFromCanvasEvents cam
  (CanvasEvents mousePosO mouseButtonE mouseScrollE viewportO _ _) =
  arcBallTransB cam mousePosO mouseButtonE viewportB mouseScrollE
  where (Observable viewportB _) = viewportO

arcBallTransB :: ArcBallCamera
              -> Observable MousePosition
              -> Event MouseButtonEvent
              -> Behavior Viewport
              -> Event MouseScrollAmount
              -> MomentIO (Behavior Transformation)
arcBallTransB cam mousePosO mouseButtonE viewportB mouseScrollE =
  do (_, dragE) <- liftMoment $ mouseDragEvents mouseButtonE mousePosO
     let dragFilteredE = filterClipE viewportB mouseDragOriginPos dragE
         (Observable mousePosB _) = mousePosO
         scrollFilteredE = filterClipB viewportB mousePosB mouseScrollE
     bCamState <- accumB (cam, cam) $
       unions [ arcBallClick <$> mouseButtonE
              , arcBallDrag <$> viewportB <@> dragFilteredE
              , arcBallScrollEvent scrollFilteredE
              ]
     return $ arcBallTrans <$> (snd <$> bCamState)

type ArcBallState = (ArcBallCamera, ArcBallCamera)

arcBallClick :: MouseButtonEvent -> ArcBallState -> ArcBallState
arcBallClick (bn, Pressed) cs@(_, cam)
  | bn == arcBallDragButton cam = (cam, cam)
  | otherwise                   = cs
arcBallClick _ cs = cs

arcBallDrag :: Viewport -> MouseDrag -> ArcBallState -> ArcBallState
arcBallDrag vp (MouseDrag opos pos bn) (ocam, cam) = (ocam, cam')
  where cam' = if bn == [arcBallDragButton cam] then newCam else cam
        newCam = arcBallMouseRotate vp opos ocam pos cam


-- | Rotates the arcball camera. This is not actually true arcball rotation.
-- Instead of thinking of a ball, think of two independent cylinders. One
-- rotates about the x axis (elevation cylinder), and one rotates about the y
-- axis (azimuth cylinder). We compute the angle changes about these cylinders
-- independently. This frees us from the nasty task of handling when the mouse
-- goes outside the bounds of the arcball.
arcBallMouseRotate :: Viewport
                   -> MousePosition -- ^ Original mouse position
                   -> ArcBallCamera -- ^ Original state
                   -> MousePosition -- ^ Current mouse position
                   -> ArcBallCamera -- ^ Current state
                   -> ArcBallCamera -- ^ New state
arcBallMouseRotate (Viewport (GL.Position vxp vyp) (GL.Size vwp vhp))
                   (MousePosition oxp oyp)
                   ocs
                   (MousePosition xp yp)
                   cs =
  cs { arcBallAzimuth = azim', arcBallElevation = elev' }
  where
    -- Compute the difference of azimuth and elevation
    da = rotateDelta vwp (oxp - vxp) (xp - vxp)
    de = rotateDelta vhp (oyp - vyp) (yp - vyp)

    -- Apply the differences
    a = arcBallAzimuth   ocs + da
    e = arcBallElevation ocs - de

    -- Don't allow azimuth and elevation out of bounds
    azim' = mod' a (pi * 2)
    elev' = min (pi / 2) $ max ((-pi) / 2) e

-- | Change in angle from two x coordinates on a circle of a given radius.
rotateDelta :: GL.GLint -- ^ Radius
            -> GL.GLint -- ^ Original x
            -> GL.GLint -- ^ New x
            -> GL.GLfloat
rotateDelta r oxp xp = a - oa
  where oa = circleAngle $ normalizeCoord oxp r
        a  = circleAngle $ normalizeCoord xp r


-- | Maps a coordinate from [0, len] to [-1, 1]
normalizeCoord :: (Integral a, Floating b) => a -> a -> b
normalizeCoord x len = fromIntegral x / fromIntegral len * 2.0 - 1.0

-- | Finds the angle of a given x coordinate about a circle, assuming the
-- radius of the circle is 1.
circleAngle :: GL.GLfloat -> GL.GLfloat
circleAngle x =
  let norm = x ** 2
      y    = if norm < 1 then sqrt (1 - norm) else 0
  in atan (x / y)

arcBallScrollEvent :: Event MouseScrollAmount
                   -> Event (ArcBallState -> ArcBallState)
arcBallScrollEvent eScroll = doZoom <$> eScroll
  where doZoom z (cm, cs) = (cm, arcBallMouseZoom cs z)


arcBallMouseZoom :: ArcBallCamera -> MouseScrollAmount -> ArcBallCamera
arcBallMouseZoom cs (MouseScrollAmount z) =
  cs { arcBallWidth = arcBallWidth cs * (1.0 - 0.1 * z) }
