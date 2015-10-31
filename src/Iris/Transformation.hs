-- | Module for arbitrary transformations of plot items

module Iris.Transformation
       ( Transformation
       , cameraTrans
       , identity
       , translation
       , scale
       , apply
       ) where

import           Data.List (foldl1')
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L

import           Iris.Camera

-- | Type used to represent a transformation on a plot item.
type Transformation = L.M44 GL.GLfloat

-- | Creates a transformation that centers from the camera center to (0, 0),
-- and scales widths and heights from the camera width/height to [-1, 1].
cameraTrans :: CameraState -> Transformation
cameraTrans (CameraState (L.V2 cx cy) w h) =
  foldl1' apply [scale', trans, identity]
  where trans  = translation (L.V3 (-cx) (-cy) 0)
        scale' = scale (L.V3 (2/w) (2/h) 1)

-- | The identity transformation does nothing to its operand.
identity :: Transformation
identity = L.identity

-- | Translates the operand to the given point.
translation :: L.V3 GL.GLfloat -> Transformation
translation (L.V3 x y z) =
  L.V4 (L.V4 1 0 0 x) (L.V4 0 1 0 y) (L.V4 0 0 1 z) (L.V4 0 0 0 1)

-- | Scales the operand uniformly along the cartesian axes. The input vector
-- gives the scaling factor along each axis.
scale :: L.V3 GL.GLfloat -> Transformation
scale (L.V3 xs ys zs) =
  L.V4 (L.V4 xs 0 0 0) (L.V4 0 ys 0 0) (L.V4 0 0 zs 0) (L.V4 0 0 0 1)

-- | Synonym for matrix multiplication.
apply :: Transformation -> Transformation -> Transformation
apply = (L.!*!)
