-- | Compose drawable items into a scene graph.

module Iris.SceneGraph
       ( SceneNode (..)
       , PlotItem (..)
       , cameraNode
       , makeScene
       ) where


import           Control.Concurrent.STM
import           Control.Lens
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import           Reactive.Banana
import           Reactive.Banana.Frameworks

import           Iris.Backends
import           Iris.Camera
import           Iris.Reactive
import           Iris.Transformation


-- | Recursive definition of a scene graph tree.
data SceneNode = Collection [SceneNode]
               | Transform (Behavior Transformation) SceneNode
               | Drawable PlotItem


-- | An wrapper around a function to plot an item.
data PlotItem = PlotItem
  { drawFunc :: Transformation -> IO ()
  }

makeScene :: (Window a) => a ->
             WindowEvents ->
             Event () ->  -- ^ Draw event
             SceneNode ->
             MomentIO ()
makeScene win events eDraw n =
  do reactimate $ (\_ -> drawRoot win) <$> eDraw
     let bTrans = aspectTrans <$> (events ^. windowSizeObservable ^. behavior)
     makeNode eDraw bTrans n
     return ()

makeNode :: Event () ->
            Behavior Transformation ->
            SceneNode ->
            MomentIO ()
makeNode eDraw bTrans (Collection ns) = mapM_ (makeNode eDraw bTrans) ns
makeNode eDraw bTrans (Drawable item) =
  do let eTrans = bTrans <@ eDraw
     reactimate $ drawFunc item <$> eTrans
     return ()
makeNode eDraw bTrans (Transform t n) = makeNode eDraw bTrans' n
  where bTrans' = liftA2 Iris.Transformation.apply bTrans t


drawRoot :: (Window a) => a -> IO ()
drawRoot win =
  do winSize <- framebufferSize win
     GL.viewport $= (GL.Position 0 0, winSize)
     GL.scissor $= Just (GL.Position 0 0, winSize)

     GL.clearColor $= GL.Color4 0 0 0 1
     GL.depthFunc $= Just GL.Less
     GL.clear [GL.ColorBuffer, GL.DepthBuffer]


-- | Creates a DynamicNode for a camera
cameraNode :: (Camera a) => Behavior a -> SceneNode -> SceneNode
cameraNode bCam = Transform (cameraTrans <$> bCam)
