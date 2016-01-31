-- | This module wraps a lot of raw OpenGL functions. A lot of this
-- functionality is inspired by Anthony Cowley's GLUtil package.

module Iris.OpenGL
       ( module X
       , drawIndexedTris
       ) where

import Foreign.Ptr (nullPtr)
import Graphics.Rendering.OpenGL

import Iris.OpenGL.AsUniform as X
import Iris.OpenGL.BufferObjects as X
import Iris.OpenGL.Errors as X
import Iris.OpenGL.ShaderProgram as X
import Iris.OpenGL.Viewport as X


-- | @drawIndexedTris n@ draws @n@ 'Triangles' using vertex data from
-- the currently bound 'ArrayBuffer' and indices from the beginning of
-- the currently bound 'ElementArrayBuffer'. Note that there must be
-- at least @n * 3@ indices in the 'ElementArrayBuffer'!
drawIndexedTris :: GLsizei -> IO ()
drawIndexedTris n = drawElements Triangles (n*3) UnsignedInt nullPtr
