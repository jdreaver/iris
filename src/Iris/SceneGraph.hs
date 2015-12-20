{-# LANGUAGE CPP #-}

-- | Compose drawable items into a scene graph.

module Iris.SceneGraph
       ( SceneNode (..)
       , DrawNode (..)
       , makeScene
       , transSceneNode
       , effectSceneNode
       , groupSceneNode
       ) where


import           Control.Lens
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

import           Iris.Backends
import           Iris.Camera
import           Iris.DrawGraph
import           Iris.Reactive
import           Iris.Transformation


-- | Recursive definition of a scene graph tree.
data SceneNode = SceneNode
  { drawNodeB :: Behavior DrawNode
  }


groupSceneNode :: [SceneNode] -> SceneNode
groupSceneNode cs = SceneNode $ groupNode <$> groupNodeB cs

groupNodeB :: [SceneNode] -> Behavior [DrawNode]
groupNodeB = sequenceA . map drawNodeB

transSceneNode :: Behavior Transformation -> [SceneNode] -> SceneNode
transSceneNode tB cs = SceneNode $ transNode <$> tB <*> groupNodeB cs

effectSceneNode :: Behavior (IO ()) -> [SceneNode] -> SceneNode
effectSceneNode f cs = SceneNode $ effectNode <$> f <*> groupNodeB cs

#if !MIN_VERSION_base(4,8,0)
sequenceA :: Applicative f => [f a] -> f [a]
sequenceA = foldr k (pure [])
  where
    k f f' = (:) <$> f <*> f'
#endif

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

     let bTrans = aspectTrans <$> (events ^. canvasSizeObservable ^. behavior)
         root' = sceneRoot win [transSceneNode bTrans [root]]

     -- Recurse through the scene graph to hook up the draw event and
     -- transformation behavior to all nodes.
     let bGraph = drawNodeB root'
     let eDraw = drawGraph <$> bGraph <@ (events ^. drawEvent)
     reactimate eDraw

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
                     let n'  = transSceneNode bCamTrans [n]
                         hs' = camHandler : hs
                     return (n', hs')


sceneRoot :: (Canvas a) => a -> [SceneNode] -> SceneNode
sceneRoot can = effectSceneNode (pure $ drawRoot can)

drawRoot :: (Canvas a) => a -> IO ()
drawRoot win =
  do winSize <- framebufferSize win
     GL.viewport $= (GL.Position 0 0, winSize)
     GL.scissor $= Just (GL.Position 0 0, winSize)

     GL.clearColor $= GL.Color4 0 0 0 1
     GL.depthFunc $= Just GL.Less
     GL.clear [GL.ColorBuffer, GL.DepthBuffer]
