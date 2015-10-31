-- | Handle camera interaction in a scene

module Iris.Camera
       ( CameraState (..)
       , CameraCenter
       ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L

-- | Vector holding the position of the center of a 2D camera.
type CameraCenter = L.V2 GL.GLfloat

-- | Container for 2D plot camera info. This is usually put in a TVar,
-- written to by interaction callbacks, and read from the OpenGL draw function
-- to create the transformation matrix.
data CameraState = CameraState
  { center :: CameraCenter
  , width  :: GL.GLfloat
  , height :: GL.GLfloat
  } deriving (Show)
