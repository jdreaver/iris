{-# LANGUAGE CPP #-}

-- | Shows how to use a clipper node to split the viewport, and how to use two
-- different cameras in the same scene.

module Main where

#if !MIN_VERSION_base(4,8,0)
import           Prelude.Compat (sequenceA)
#endif

import qualified Graphics.Rendering.OpenGL as GL

import           Iris.Backends
import qualified Iris.Backends.GLFW as W
import           Iris.Camera
import           Iris.Reactive
import           Iris.SceneGraph
import           Iris.Transformation
import           Iris.Util


main :: IO ()
main =
  do win <- W.makeWindow "Clipping" (480 * 2, 480)
     canvas <- W.initGLFW win

     network <- compile $ makeNetwork canvas
     actuate network

     W.mainLoop canvas


makeNetwork :: (Canvas a) => a -> MomentIO ()
makeNetwork can =
  do (viewB1, viewB2) <- clipBehaviors can
     events <- makeEvents can
     let cam = arcBallCamera { arcBallWidth     = 6
                             , arcBallAzimuth   = 30 * pi / 180
                             , arcBallElevation = 30 * pi / 180
                             }
     camB1 <- cameraTransB cam events viewB1
     camB2 <- cameraTransB cam events viewB2
     cube <- liftIO makeCube
     let node1 = flip transNode cube <$> camB1
         clip1 = clipperNode  <$> viewB1 <*> sequenceA [node1]
         node2 = flip transNode cube <$> camB2
         clip2 = clipperNode  <$> viewB2 <*> sequenceA [node2]
         scene = groupNode <$> sequenceA [clip1, clip2]
     makeScene can scene (Nothing :: Maybe ArcBallCamera)


-- | Create a Behavior for the viewport sizes. Each viewport takes up half of
-- the screen.
clipBehaviors :: (Canvas a) => a -> MomentIO (Behavior Viewport, Behavior Viewport)
clipBehaviors c =
  do events <- makeEvents c
     let buffSizeB = observableBehavior $ framebufferSizeObservable events
         f1 (FramebufferSize w h) = Viewport (GL.Position 0 0) (GL.Size (w `quot` 2) h)
         f2 (FramebufferSize w h) = Viewport (GL.Position (w `quot` 2) 0) (GL.Size (w `quot` 2) h)
     return (f1 <$> buffSizeB, f2 <$> buffSizeB)

cameraTransB :: ArcBallCamera
             -> CanvasEvents
             -> Behavior Viewport
             -> MomentIO (Behavior Transformation)
cameraTransB cam (CanvasEvents mousePosO mouseButtonE mouseScrollE _ _ _) viewportB =
  arcBallTransB cam mousePosO mouseButtonE viewportB mouseScrollE
