{-# LANGUAGE CPP #-}

-- | Compose drawable items into a scene graph.

module Iris.SceneGraph.DynamicScene
       ( DrawNode (..)
       , makeScene
       , sceneWithCamera
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

makeScene :: (Canvas a)
          => a
          -> DynamicDrawNode
          -> MomentIO ()
makeScene win root =
  do events <- makeEvents win
     makeScene' win events root

sceneWithCamera :: (Canvas a, Camera c)
                => a
                -> DynamicDrawNode
                -> c
                -> MomentIO ()
sceneWithCamera win n cam =
  do events <- makeEvents win
     root <- attachCam events n cam
     makeScene' win events root

makeScene' :: (Canvas a)
           => a
           -> CanvasEvents
           -> DynamicDrawNode
           -> MomentIO ()
makeScene' win events root =
  do let root' = sceneRoot win <$> root
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
    do bCamTrans <- camFromEventsB cam es
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
