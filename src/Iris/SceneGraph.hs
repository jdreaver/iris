-- | Compose drawable items into a scene graph.

module Iris.SceneGraph
       ( Scene (..)
       , SceneNode (..)
       , PlotItem (..)
       , drawScene
       ) where


import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

import           Iris.Camera
import           Iris.Transformation

-- | Combination of a scene root and camera for that scene
data Scene = Scene
  { sceneRoot :: SceneNode
  , camera    :: CameraState
  }

-- | Recursive definition of a scene graph tree.
data SceneNode = Collection [SceneNode]
               | Transform Transformation SceneNode
               | Drawable PlotItem

-- | An wrapper around a function to plot an item.
data PlotItem = PlotItem
  { drawFunc :: Transformation -> IO ()
  }

-- | Traverse the scene and draw each item.
drawScene :: Scene -> IO ()
drawScene (Scene root cam) =
  do GL.clearColor $= GL.Color4 0 0 0 1
     GL.depthFunc $= Just GL.Less
     GL.clear [GL.ColorBuffer, GL.DepthBuffer]

     let t = cameraTrans cam
     drawNode t root


drawNode :: Transformation -> SceneNode -> IO ()
drawNode t (Collection items) = mapM_ (drawNode t) items
drawNode t (Transform t' n) = drawNode (t `apply` t') n
drawNode t (Drawable item) = drawFunc item t
