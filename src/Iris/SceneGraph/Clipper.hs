-- | Clips the view region.

module Iris.SceneGraph.Clipper
       ( clipperNode
       , drawClipper
       , clip
       ) where

import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

import           Iris.SceneGraph.DrawGraph

-- | Create a node that clips the viewport to a given size, draws children, and
-- then restores the original viewport size.
clipperNode :: Viewport -> DrawNode -> DrawNode
clipperNode v child = DrawNode $ drawClipper v child

drawClipper :: Viewport -> DrawNode -> DrawFunc
drawClipper v' (DrawNode childFunc) drawDat =
  do clip v'
     childFunc drawDat
     clip (viewport drawDat)

-- | Run GL.viewport and GL.scissor with a given size
clip :: Viewport -> IO ()
clip (Viewport p s) =
  do GL.viewport $= (p, s)
     GL.scissor $= Just (p, s)
