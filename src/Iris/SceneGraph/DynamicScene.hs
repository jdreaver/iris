{-# LANGUAGE CPP #-}

-- | Compose drawable items into a scene graph.

module Iris.SceneGraph.DynamicScene
       ( DrawNode (..)
       , makeScene
       , sceneRoot
       ) where

import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

import           Iris.Backends
import           Iris.Camera
import           Iris.Reactive
import           Iris.SceneGraph.Clipper
import           Iris.SceneGraph.DrawGraph


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

     let root' = sceneRoot win <$> root
         eDraw = drawGraph <$> root' <@ drawEvent events
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
    do bCamTrans <- cameraTransBehavior cam es
       return $ transNode <$> bCamTrans <*> n


sceneRoot :: (Canvas a) => a -> DrawNode -> DrawNode
sceneRoot can = effectNode (drawRoot can)

drawRoot :: (Canvas a) => a -> IO ()
drawRoot win =
  do GL.clearColor $= GL.Color4 0 0 0 1
     GL.depthFunc $= Just GL.Lequal
     GL.blend $= GL.Enabled
     GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

     (FramebufferSize fbw fbh) <- framebufferSize win
     clip (Viewport (GL.Position 0 0) (GL.Size fbw fbh))

     GL.clear [GL.ColorBuffer, GL.DepthBuffer]
