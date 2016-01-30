-- | Module for arbitrary transformations of plot items

module Iris.Transformation
       ( Transformation
       , identity
       , translation
       , scale
       , apply
       , aspectTrans
       , rotateX
       , rotateZ
       ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L

import           Iris.OpenGL (Viewport (..))

-- | Type used to represent a transformation on a plot item.
type Transformation = L.M44 GL.GLfloat

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

-- | Scales a view so the lengths are invariant to the window aspect ratio. If
-- we have a viewport aspect ratio not equal to one, then the clip coordinates
-- of -1 to 1 will have pixel lengths, making an image look distorted.
aspectTrans :: Viewport -> Transformation
aspectTrans (Viewport _ (GL.Size w h))
  | aspect >= 1 = scale $ L.V3 1 aspect 1
  | otherwise   = scale $ L.V3 (1 / aspect) 1 1
  where aspect = fromIntegral w / fromIntegral h

rotateX :: GL.GLfloat -> Transformation
rotateX angle =
  L.V4 (L.V4 1 0 0 0) (L.V4 0 ca (-sa) 0) (L.V4 0 sa ca 0) (L.V4 0 0 0 1)
  where ca = cos angle
        sa = sin angle

rotateZ :: GL.GLfloat -> Transformation
rotateZ angle =
  L.V4 (L.V4 ca (-sa) 0 0) (L.V4 sa ca 0 0) (L.V4 0 0 1 0) (L.V4 0 0 0 1)
  where ca = cos angle
        sa = sin angle
