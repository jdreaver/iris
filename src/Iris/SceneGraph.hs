-- | Compose drawable items into a scene graph.

module Iris.SceneGraph
       ( SceneNode (..)
       , PlotItem (..)
       , drawScene
       , cameraNode
       ) where


import           Control.Concurrent.STM
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

import           Iris.Backends
import           Iris.Camera
import           Iris.Transformation


-- | Recursive definition of a scene graph tree.
data SceneNode = Collection [SceneNode]
               | Transform Transformation SceneNode
               | Drawable PlotItem
               | DynamicNode (IO SceneNode)


-- | An wrapper around a function to plot an item.
data PlotItem = PlotItem
  { drawFunc :: Transformation -> IO ()
  }

-- | Traverse the scene and draw each item.
drawScene :: (Window w) => w -> SceneNode -> IO ()
drawScene win root =
  do winSize <- framebufferSize win
     GL.viewport $= (GL.Position 0 0, winSize)

     GL.clearColor $= GL.Color4 0 0 0 1
     GL.depthFunc $= Just GL.Less
     GL.clear [GL.ColorBuffer, GL.DepthBuffer]

     let at = aspectTrans winSize
     drawNode at root


drawNode :: Transformation -> SceneNode -> IO ()
drawNode t (Collection items) = mapM_ (drawNode t) items
drawNode t (Transform t' n) = drawNode (t `apply` t') n
drawNode t (Drawable item) = drawFunc item t
drawNode t (DynamicNode f) = f >>= drawNode t


cameraNode :: (Camera a) => TVar a -> SceneNode -> SceneNode
cameraNode camTVar child = DynamicNode f
  where f :: IO SceneNode
        f = do c <- readTVarIO camTVar
               return $ Transform (cameraTrans c) child
