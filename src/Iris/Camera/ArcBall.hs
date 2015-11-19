-- | Definition of an arcball camera for use in 3D scenes

module Iris.Camera.ArcBall
       ( ArcBallCamera (..)
       , arcBallCamera
       , cameraTrans
       ) where

import           Data.List (foldl1')
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L

import           Iris.Backends
import           Iris.Camera.Class
import           Iris.Reactive
import           Iris.Transformation

-- | Camera that rotates about a central point
data ArcBallCamera = ArcBallCamera
  { arcBallCenter    :: L.V2 GL.GLfloat
  , arcBallWidth     :: GL.GLfloat
  , arcBallAzimuth   :: GL.GLfloat -- ^ Azimuth in radians
  , arcBallElevation :: GL.GLfloat -- ^ Elevation in radians
  }

instance Camera ArcBallCamera where
  initCamera = initCamera'

arcBallCamera :: ArcBallCamera
arcBallCamera = ArcBallCamera (L.V2 0 0) 2 0 0

cameraTrans :: ArcBallCamera -> Transformation
cameraTrans (ArcBallCamera (L.V2 cx cy) w a e) =
  foldl1' Iris.Transformation.apply [scale', rotElev, rotAzim, trans, identity]
  where trans    = translation (L.V3 (-cx) (-cy) 0)
        rotAzim  = rotateZ a
        rotElev  = rotateX e
        scale'   = scale (L.V3 (2/w) (2/w) (2/1000))

initCamera' :: ArcBallCamera ->
               CanvasEvents ->
               MomentIO (Behavior Transformation, CanvasEventHandler)
initCamera' c _ = return (pure $ cameraTrans c, canvasEventHandler)
