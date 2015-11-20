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
  do (bCam, hCam) <- newBehavior cam

     bPressedButtons <- liftMoment $ recordButtons events bCam

     (ePos, hPos) <- eventHandler NotAccepted
     let eMovedCam = dragEvent events bPressedButtons bCam ePos
     reactimate $ hCam <$> eMovedCam

     (eScroll, hScroll) <- eventHandler NotAccepted
     let eScrolledCam = scrollEvent bCam eScroll
     reactimate $ hCam <$> eScrolledCam

     let winEventHandler = canvasEventHandler
                           { mousePosEventHandler    = Just hPos
                           , mouseScrollEventHandler = Just hScroll
                           }

     return (cameraTrans <$> bCam, winEventHandler)


dragEvent :: CanvasEvents ->
             Behavior (PressedButtons ArcBallCamera) ->
             Behavior ArcBallCamera ->
             Event GL.Position ->
             Event ArcBallCamera
dragEvent events bPressedButtons bCam ePos =
  doMove <$> bPressedButtons
         <*> events ^. canvasSizeObservable ^. behavior
         <*> bCam
         <@> ePos
  where doMove pbs size cs pos = maybe cs
                                 (\bs' -> mouseRotate size pos bs' cs)
                                 (Map.lookup (arcBallDragButton cs) (buttonMap pbs))


-- | Rotates the arcball camera.
mouseRotate :: GL.Size ->
               GL.Position ->
               (GL.Position, ArcBallCamera) ->
               ArcBallCamera ->
               ArcBallCamera
mouseRotate (GL.Size w h) (GL.Position x y) (GL.Position ox oy, ocs) cs =
  cs { arcBallAzimuth = azim', arcBallElevation = elev' }
  where
    -- Compute normalized x and y coordinates, both for the original mouse
    -- position and the current mouse position.
    (ox', oy') = (normalizeCoord ox w, normalizeCoord oy h)
    (x' , y' ) = (normalizeCoord x  w, normalizeCoord y  h)

    -- Since we have normalized our click coordinates, we can assume our
    -- imaginary arcball has a radius of 1. We can compute the z coordinates of
    -- the clicks using the Pythagorean theorem. Note that if the user clicked
    -- near a corner, we assume that z is zero, and they have clicked the edge
    -- of the arcball.
    oxy = ox' ** 2 + oy' ** 2
    xy  = x'  ** 2 + y'  ** 2
    oz' = if oxy < 1 then sqrt (1 - oxy) else 0
    z'  = if xy  < 1 then sqrt (1 - xy)  else 0

    -- Compute the original and new azimuth and elevations of the clicks. The
    -- azimuth spans the x-z plane, and the elevation spans the z-y plane.
    oa = atan (ox' / oz')
    oe = atan (oy' / oz')
    a  = atan (x'  / z' )
    e  = atan (y'  / z' )

    a' = arcBallAzimuth   ocs + (a - oa)
    e' = arcBallElevation ocs + (e - oe)
    azim' = mod' a' (pi * 2)
    elev' = min (pi / 2) $ max ((-pi) / 2) e'


-- | Maps a coordinate from [0, len] to [-1, 1]
normalizeCoord :: (Integral a, Floating b) => a -> a -> b
normalizeCoord x len = fromIntegral x / fromIntegral len * 2.0 - 1.0


scrollEvent :: Behavior ArcBallCamera ->
               Event GL.GLfloat ->
               Event ArcBallCamera
scrollEvent bCam eScroll =
  mouseZoom <$> bCam <@> eScroll


mouseZoom :: ArcBallCamera -> GL.GLfloat -> ArcBallCamera
mouseZoom cs z = cs { arcBallWidth = arcBallWidth cs * (1.0 - 0.1 * z) }
