-- | Compose drawable items into a scene graph.

module Iris.SceneGraph
       ( Scene (..)
       , SceneNode (..)
       , PlotItem (..)
       , drawScene
       ) where


import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

import           Iris.Backends
import           Iris.Camera
import           Iris.Transformation

-- | Combination of a scene root and camera for that scene
data (Camera a) => Scene a = Scene
  { sceneRoot :: SceneNode
  , camera    :: a
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
drawScene :: (Window w, Camera b) => w -> Scene b -> IO ()
drawScene win (Scene root cam) =
  do winSize <- framebufferSize win
     GL.viewport $= (GL.Position 0 0, winSize)

     GL.clearColor $= GL.Color4 0 0 0 1
     GL.depthFunc $= Just GL.Less
     GL.clear [GL.ColorBuffer, GL.DepthBuffer]

     let ct = cameraTrans cam
         at = aspectTrans winSize
         t  = at `apply` ct
     drawNode t root


drawNode :: Transformation -> SceneNode -> IO ()
drawNode t (Collection items) = mapM_ (drawNode t) items
drawNode t (Transform t' n) = drawNode (t `apply` t') n
drawNode t (Drawable item) = drawFunc item t
