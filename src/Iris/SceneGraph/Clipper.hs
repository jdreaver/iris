-- | Clips the view region.

module Iris.SceneGraph.Clipper
       ( clipperNode
       , drawClipper
       ) where

import           Iris.OpenGL
import           Iris.SceneGraph.DrawGraph

-- | Create a node that clips the viewport to a given size, draws children, and
-- then restores the original viewport size.
clipperNode :: Viewport -> DrawNode -> DrawNode
clipperNode v child = DrawNode $ drawClipper v child

drawClipper :: Viewport -> DrawNode -> DrawFunc
drawClipper v' (DrawNode childFunc) drawDat =
  do clipViewport v'
     childFunc drawDat
     clipViewport (viewport drawDat)
