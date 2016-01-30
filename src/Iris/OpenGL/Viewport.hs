-- | Here, we simply define a Viewport datatype and some convenience functions
-- for using it.

module Iris.OpenGL.Viewport
       ( Viewport (..)
       , insideViewport
       , clipViewport
       ) where

import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.Rendering.OpenGL (($=))

-- | Holds the position of the top-left corner and the width/height of a
-- viewport.
data Viewport = Viewport
  { viewportPos  :: !GL.Position
  , viewportSize :: !GL.Size
  } deriving (Show)

-- | Determines if a given point is inside of a viewport.
insideViewport :: Viewport -> GL.GLint -> GL.GLint -> Bool
insideViewport (Viewport (GL.Position px py) (GL.Size pw ph)) x y =
  x >= px && x <= px + pw && y >= py && y <= py + ph

-- | Run GL.viewport and GL.scissor with a given size
clipViewport :: Viewport -> IO ()
clipViewport (Viewport p s) =
  do GL.viewport $= (p, s)
     GL.scissor $= Just (p, s)
