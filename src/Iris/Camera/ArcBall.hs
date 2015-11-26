-- | Definition of an arcball camera for use in 3D scenes

module Iris.Camera.ArcBall
       ( ArcBallCamera (..)
       , arcBallCamera
       , cameraTrans
       ) where

import           Control.Lens
import           Data.Fixed (mod')
import           Data.List (foldl1')
import qualified Data.Map.Strict as Map
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L

import           Iris.Backends
import           Iris.Camera.Class
import           Iris.Events
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
  initCamera = initCamera'

arcBallCamera :: ArcBallCamera
arcBallCamera = ArcBallCamera (L.V3 0 0 0) 2 0 0 MouseButtonLeft

cameraTrans :: ArcBallCamera -> Transformation
cameraTrans (ArcBallCamera (L.V3 cx cy cz) w a e _) =
  foldl1' Iris.Transformation.apply [scale', rotElev, rotAzim, trans, identity]
  where trans    = translation (L.V3 (-cx) (-cy) (-cz))
        rotAzim  = rotateZ a
        rotElev  = rotateX ((pi / 2) - e)
        scale'   = scale (L.V3 (2/w) (2/w) (2/1000))

initCamera' :: ArcBallCamera ->
               CanvasEvents ->
               MomentIO (Behavior Transformation, CanvasEventHandler)
initCamera' cam events =
  do (ePos, hPos) <- eventHandler NotAccepted
     (eScroll, hScroll) <- eventHandler NotAccepted
     ePressedButtons <- liftMoment $ recordButtons events

     let eMovedCam = dragEvent events ePos
         eScrolledCam = scrollEvent eScroll
         eClick = clickEvent ePressedButtons

         winEventHandler = canvasEventHandler
                           { mousePosEventHandler    = Just hPos
                           , mouseScrollEventHandler = Just hScroll
                           }

     bCamState <- accumB (Map.fromList [], cam) $ unions [ eClick, eMovedCam, eScrolledCam ]

     return (cameraTrans <$> (snd <$> bCamState), winEventHandler)

type ArcBallState = (Map.Map MouseButton (ArcBallCamera, GL.Position), ArcBallCamera)

clickEvent :: Event PressedButtons ->
              Event (ArcBallState -> ArcBallState)
clickEvent ePressedButtons = f <$> ePressedButtons
  where f pb (_, cs) = (fmap ((,) cs) pb, cs)

dragEvent :: CanvasEvents ->
             Event GL.Position ->
             Event (ArcBallState -> ArcBallState)
dragEvent events ePos =
  doMove <$> events ^. canvasSizeObservable ^. behavior
         <@> ePos
  where doMove size pos (cm, cs) =
          maybe (cm, cs)
          (\(c1, opos) -> (cm, mouseRotate size pos opos c1 cs))
          (Map.lookup (arcBallDragButton cs) cm)


-- | Rotates the arcball camera. This is not actually true arcball rotation.
-- Instead of thinking of a ball, think of two independent cylinders. One
-- rotates about the x axis (elevation cylinder), and one rotates about the y
-- axis (azimuth cylinder). We compute the angle changes about these cylinders
-- independently. This frees us from the nasty task of handling when the mouse
-- goes outside the bounds of the arcball.
mouseRotate :: GL.Size ->
               GL.Position ->
               GL.Position ->
               ArcBallCamera ->
               ArcBallCamera ->
               ArcBallCamera
mouseRotate (GL.Size w h) (GL.Position x y) (GL.Position ox oy) ocs cs =
  cs { arcBallAzimuth = azim', arcBallElevation = elev' }
  where
    -- Compute normalized x and y coordinates, both for the original mouse
    -- position and the current mouse position.
    (ox', oy') = (normalizeCoord ox w, normalizeCoord oy h)
    (x' , y' ) = (normalizeCoord x  w, normalizeCoord y  h)

    -- Compute the original and new azimuth and elevations of the clicks. The
    -- azimuth spans the x-z plane, and the elevation spans the z-y plane.
    oa = circleAngle ox'
    oe = circleAngle oy'
    a  = circleAngle x'
    e  = circleAngle y'

    -- da = acos $ (oxp * xp) + (ozp * zp)
    -- de = acos $ (oyp * yp) + (ozp * zp)
    -- angle = acos $ min 1 (oxp * xp + oyp * xp + ozp * zp)

    a' = arcBallAzimuth   ocs + (a - oa)
    e' = arcBallElevation ocs + (e - oe)
    azim' = mod' a' (pi * 2)
    elev' = min (pi / 2) $ max ((-pi) / 2) e'


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

scrollEvent :: Event GL.GLfloat ->
               Event (ArcBallState -> ArcBallState)
scrollEvent eScroll = doZoom <$> eScroll
  where doZoom z (cm, cs) = (cm, mouseZoom cs z)


mouseZoom :: ArcBallCamera -> GL.GLfloat -> ArcBallCamera
mouseZoom cs z = cs { arcBallWidth = arcBallWidth cs * (1.0 - 0.1 * z) }
