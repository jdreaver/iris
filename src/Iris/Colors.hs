-- | Common color specifications and utilites

module Iris.Colors
       ( Color
       ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L

-- | Simple type synonym for colors
type Color = L.V3 GL.GLfloat
