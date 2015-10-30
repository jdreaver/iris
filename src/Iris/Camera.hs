-- | Handle camera interaction in a scene

module Iris.Camera where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L

type CameraCenter = L.V2 GL.GLfloat

data CameraState = CameraState
  { center :: CameraCenter
  , width  :: GL.GLfloat
  , height :: GL.GLfloat
  } deriving (Show)
