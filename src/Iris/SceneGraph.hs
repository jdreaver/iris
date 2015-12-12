-- | Compose drawable items into a scene graph.

module Iris.SceneGraph
       ( SceneNode (..)
       , makeScene

       , Effect (..)
       , Visual (..)
       ) where


import           Control.Lens
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

import           Iris.Backends
import           Iris.Camera
import           Iris.Reactive
import           Iris.Transformation


-- | Recursive definition of a scene graph tree.
data SceneNode = Collection [SceneNode]
               | Transform (Behavior Transformation) SceneNode
               | EffectNode Effect SceneNode
               | VisualNode Visual


data Visual = Visual
  { drawEventFunc :: Event () -> Behavior Transformation -> MomentIO ()
  }

data Effect = Effect (Event () -> MomentIO ())

makeScene :: (Canvas a, Camera c) =>
             a ->
             SceneNode ->
             Maybe c ->
             MomentIO ()
makeScene win n maybeCam =
  do events <- makeEvents win

     -- Initialize camera and attach event handlers
     (root, winEventHandlers) <- attachCam maybeCam events n []
     attachEventHandlers events winEventHandlers

     let root' = sceneRoot win root

     -- Recurse through the scene graph to hook up the draw event and
     -- transformation behavior to all nodes.
     let bTrans = aspectTrans <$> (events ^. canvasSizeObservable ^. behavior)
     makeNode (events ^. drawEvent) bTrans root'

     return ()

-- | If we have a camera, then initialize the reactive form of the camera, set
-- the camera node as root, and make the camera event handler the first event
-- handler.
attachCam :: (Camera c) =>
             Maybe c ->
             CanvasEvents ->
             SceneNode ->
             [CanvasEventHandler] ->
             MomentIO (SceneNode, [CanvasEventHandler])
attachCam maybeCam es n hs =
  case maybeCam of
    Nothing    -> return (n, hs)
    (Just cam) -> do (bCamTrans, camHandler) <- initCamera cam es
                     let n'  = Transform bCamTrans n
                         hs' = camHandler : hs
                     return (n', hs')


makeNode :: Event () ->
            Behavior Transformation ->
            SceneNode ->
            MomentIO ()
makeNode eDraw bTrans (Collection ns) = mapM_ (makeNode eDraw bTrans) ns
makeNode eDraw bTrans (EffectNode (Effect e) n) = e eDraw >> makeNode eDraw bTrans n
makeNode eDraw bTrans (VisualNode visual) = drawEventFunc visual eDraw bTrans
makeNode eDraw bTrans (Transform t n) = makeNode eDraw bTrans' n
  where bTrans' = liftA2 Iris.Transformation.apply bTrans t

sceneRoot :: (Canvas a) => a -> SceneNode -> SceneNode
sceneRoot can = EffectNode (Effect f)
  where f :: Event () -> MomentIO ()
        f e = reactimate $ (\_ -> drawRoot can) <$> e

drawRoot :: (Canvas a) => a -> IO ()
drawRoot win =
  do winSize <- framebufferSize win
     GL.viewport $= (GL.Position 0 0, winSize)
     GL.scissor $= Just (GL.Position 0 0, winSize)

     GL.clearColor $= GL.Color4 0 0 0 1
     GL.depthFunc $= Just GL.Less
     GL.clear [GL.ColorBuffer, GL.DepthBuffer]
