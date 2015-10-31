-- | Compose drawable items into a scene graph.

module Iris.SceneGraph
       ( Scene (..)
       , SceneNode (..)
       , PlotItem (..)
       , drawScene
       , translation
       ) where


import           Data.List (foldl1')
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L

import           Iris.Camera

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


type Transformation = L.M44 GL.GLfloat


cameraTrans :: CameraState -> Transformation
cameraTrans (CameraState (L.V2 cx cy) w h) =
  foldl1' apply [scale', trans, identity]
  where trans  = translation (L.V3 (-cx) (-cy) 0)
        scale' = scale (L.V3 (2/w) (2/h) 1)

identity :: Transformation
identity = L.identity


translation :: L.V3 GL.GLfloat -> Transformation
translation (L.V3 x y z) =
  L.V4 (L.V4 1 0 0 x) (L.V4 0 1 0 y) (L.V4 0 0 1 z) (L.V4 0 0 0 1)

scale :: L.V3 GL.GLfloat -> Transformation
scale (L.V3 xs ys zs) =
  L.V4 (L.V4 xs 0 0 0) (L.V4 0 ys 0 0) (L.V4 0 0 zs 0) (L.V4 0 0 0 1)


apply :: Transformation -> Transformation -> Transformation
apply = (L.!*!)
