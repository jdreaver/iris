{-# LANGUAGE CPP #-}

-- | Compose drawable items into a scene graph.

module Iris.SceneGraph.DynamicScene
       ( DrawNode (..)
       , makeScene
       , sceneRoot
       ) where

#if !MIN_VERSION_base(4,8,0)
import           Prelude.Compat (sequenceA)
#endif

import           Control.Lens
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

import           Iris.Backends
import           Iris.Camera
import           Iris.Reactive
import           Iris.SceneGraph.Clipper
import           Iris.SceneGraph.DrawGraph
import           Iris.Transformation


type DynamicDrawNode = Behavior DrawNode

makeScene :: (Canvas a, Camera c) =>
             a ->
             DynamicDrawNode ->
             Maybe c ->
             MomentIO ()
makeScene win n maybeCam =
  do events <- makeEvents win

     -- Initialize camera and attach event handlers
     (root, winEventHandlers) <- attachCam maybeCam events n []
     attachEventHandlers events winEventHandlers

     let bTrans = aspectTrans <$> (events ^. canvasSizeObservable ^. behavior)
         tNode = transNode <$> bTrans <*> sequenceA [root]
         root' = sceneRoot win <$> sequenceA [tNode]

     -- Recurse through the scene graph to hook up the draw event and
     -- transformation behavior to all nodes.
     let eDraw = drawGraph <$> root' <@ (events ^. drawEvent)
     reactimate eDraw

-- | If we have a camera, then initialize the reactive form of the camera, set
-- the camera node as root, and make the camera event handler the first event
-- handler.
attachCam :: (Camera c) =>
             Maybe c ->
             CanvasEvents ->
             DynamicDrawNode ->
             [CanvasEventHandler] ->
             MomentIO (DynamicDrawNode, [CanvasEventHandler])
attachCam maybeCam es n hs =
  case maybeCam of
    Nothing    -> return (n, hs)
    (Just cam) -> do (bCamTrans, camHandler) <- initCamera cam es
                     let n'  = transNode <$> bCamTrans <*> sequenceA [n]
                         hs' = camHandler : hs
                     return (n', hs')


sceneRoot :: (Canvas a) => a -> [DrawNode] -> DrawNode
sceneRoot can = effectNode (drawRoot can)

drawRoot :: (Canvas a) => a -> IO ()
drawRoot win =
  do GL.clearColor $= GL.Color4 0 0 0 1
     GL.depthFunc $= Just GL.Lequal
     GL.blend $= GL.Enabled
     GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

     winSize <- framebufferSize win
     clip (Viewport (GL.Position 0 0) winSize)

     GL.clear [GL.ColorBuffer, GL.DepthBuffer]
