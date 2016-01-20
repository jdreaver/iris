{-# LANGUAGE CPP #-}

-- | Compose drawable items into a scene graph.

module Iris.SceneGraph.DynamicScene
       ( DrawNode (..)
       , makeScene
       , sceneRoot
       ) where

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
     root <- maybe (return n) (attachCam events n) maybeCam

     let bTrans = aspectTrans <$> (events ^. canvasSizeObservable ^. behavior)
         tNode = transNode <$> bTrans <*> root
         root' = sceneRoot win <$> tNode

     -- Hook up the draw event to drawGraph
     let eDraw = drawGraph <$> root' <@ (events ^. drawEvent)
     reactimate eDraw

-- | If we have a camera, then initialize the reactive form of the camera, set
-- the camera node as root, and make the camera event handler the first event
-- handler.
attachCam :: (Camera c) =>
             CanvasEvents ->
             DynamicDrawNode ->
             c ->
             MomentIO DynamicDrawNode
attachCam es n cam =
    do bCamTrans <- initCamera cam es
       return $ transNode <$> bCamTrans <*> n


sceneRoot :: (Canvas a) => a -> DrawNode -> DrawNode
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
