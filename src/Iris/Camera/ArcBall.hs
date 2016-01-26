-- | Definition of an arcball camera for use in 3D scenes

module Iris.Camera.ArcBall
       ( ArcBallCamera (..)
       , arcBallCamera
       ) where

import           Data.Fixed (mod')
import           Data.List (foldl1')
import qualified Data.Map.Strict as Map
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L

import           Iris.Backends
import           Iris.Camera.Class
import           Iris.Mouse
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
  cameraTrans         = arcBallCameraTrans
  cameraTransBehavior = arcBallCameraTransB

arcBallCamera :: ArcBallCamera
arcBallCamera = ArcBallCamera (L.V3 0 0 0) 2 0 0 MouseButtonLeft

arcBallCameraTrans :: ArcBallCamera -> Transformation
arcBallCameraTrans (ArcBallCamera (L.V3 cx cy cz) w a e _) =
  foldl1' Iris.Transformation.apply [scale', rotElev, rotAzim, trans, identity]
  where trans    = translation (L.V3 (-cx) (-cy) (-cz))
        rotAzim  = rotateZ a
        rotElev  = rotateX ((pi / 2) - e)
        scale'   = scale (L.V3 (2/w) (2/w) (2/1000))

arcBallCameraTransB :: ArcBallCamera
                    -> CanvasEvents
                    -> MomentIO (Behavior Transformation)
arcBallCameraTransB cam events@(CanvasEvents mousePosO mouseButtonE _ _ _ _) =
  do let mousePosB = observableBehavior mousePosO
     ePressedButtons <- liftMoment $ recordButtons mousePosB mouseButtonE

     let eMovedCam = arcBallDragEvent events
         eScrolledCam = arcBallScrollEvent (mouseScrollEvent events)
         eClick = arcBallClickEvent ePressedButtons

     bCamState <- accumB (Map.fromList [], cam) $ unions [ eClick, eMovedCam, eScrolledCam ]

     return (arcBallCameraTrans <$> (snd <$> bCamState))

type ArcBallState = (Map.Map MouseButton (ArcBallCamera, MousePosition), ArcBallCamera)

arcBallClickEvent :: Event PressedButtons
                  -> Event (ArcBallState -> ArcBallState)
arcBallClickEvent ePressedButtons = f <$> ePressedButtons
  where f pb (_, cs) = (fmap ((,) cs) pb, cs)

arcBallDragEvent :: CanvasEvents
                 -> Event (ArcBallState -> ArcBallState)
arcBallDragEvent events =
  doMove <$> observableBehavior (canvasSizeObservable events)
         <@> observableEvent (mousePosObservable events)
  where doMove size pos (cm, cs) =
          maybe (cm, cs)
          (\(cso, opos) -> (cm, arcBallMouseRotate size opos cso pos cs))
          (Map.lookup (arcBallDragButton cs) cm)


-- | Rotates the arcball camera. This is not actually true arcball rotation.
-- Instead of thinking of a ball, think of two independent cylinders. One
-- rotates about the x axis (elevation cylinder), and one rotates about the y
-- axis (azimuth cylinder). We compute the angle changes about these cylinders
-- independently. This frees us from the nasty task of handling when the mouse
-- goes outside the bounds of the arcball.
arcBallMouseRotate :: CanvasSize
                   -> MousePosition -- ^ Original mouse position
                   -> ArcBallCamera -- ^ Original state
                   -> MousePosition -- ^ Current mouse position
                   -> ArcBallCamera -- ^ Current state
                   -> ArcBallCamera -- ^ New state
arcBallMouseRotate (CanvasSize wp hp) (MousePosition oxp oyp) ocs (MousePosition xp yp) cs =
  cs { arcBallAzimuth = azim', arcBallElevation = elev' }
  where
    -- Compute the difference of azimuth and elevation
    da = rotateDelta wp oxp xp
    de = rotateDelta hp oyp yp

    -- Apply the differences
    a = arcBallAzimuth   ocs + da
    e = arcBallElevation ocs + de

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
