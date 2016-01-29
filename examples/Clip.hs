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
  do win <- W.makeWindow "Clipping" (860, 860)
     canvas <- W.initGLFW win

     network <- compile $ makeNetwork canvas
     actuate network

     W.mainLoop canvas


makeNetwork :: (Canvas a) => a -> MomentIO ()
makeNetwork can =
  do (viewB1, viewB2, viewB3, viewB4) <- clipBehaviors can
     events <- makeEvents can
     cube <- liftIO makeCube
     clip1 <- arcBallNode viewB1 events cube
     clip2 <- arcBallNode viewB2 events cube
     clip3 <- arcBallNode viewB3 events cube
     clip4 <- arcBallNode viewB4 events cube
     let scene = groupNode <$> sequenceA [clip1, clip2, clip3, clip4]
     makeScene can scene (Nothing :: Maybe ArcBallCamera)


-- | Create a Behavior for the viewport sizes. Each viewport takes up half of
-- the screen.
clipBehaviors :: (Canvas a)
              => a
              -> MomentIO (Behavior Viewport, Behavior Viewport,
                           Behavior Viewport, Behavior Viewport)
clipBehaviors c =
  do events <- makeEvents c
     let buffSizeB = observableBehavior $ framebufferSizeObservable events
         sizeFunc w h = GL.Size (w `quot` 2) (h `quot` 2)
         f1 (FramebufferSize w h) = Viewport (GL.Position 0 0) (sizeFunc w h)
         f2 (FramebufferSize w h) = Viewport (GL.Position (w `quot` 2) 0) (sizeFunc w h)
         f3 (FramebufferSize w h) = Viewport (GL.Position 0 (h `quot` 2)) (sizeFunc w h)
         f4 (FramebufferSize w h) = Viewport (GL.Position (w `quot` 2) (h `quot` 2)) (sizeFunc w h)
     return (f1 <$> buffSizeB, f2 <$> buffSizeB, f3 <$> buffSizeB, f4 <$> buffSizeB)

cameraTransB :: ArcBallCamera
             -> CanvasEvents
             -> Behavior Viewport
             -> MomentIO (Behavior Transformation)
cameraTransB cam (CanvasEvents mousePosO mouseButtonE mouseScrollE _ _ _) viewportB =
  arcBallTransB cam mousePosO mouseButtonE viewportB mouseScrollE


arcBallNode :: Behavior Viewport -- ^ Viewport for this scene
            -> CanvasEvents
            -> DrawNode          -- ^ Thing to draw
            -> MomentIO (Behavior DrawNode)
arcBallNode viewportB events item =
  do let cam = arcBallCamera { arcBallWidth     = 6
                             , arcBallAzimuth   = 30 * pi / 180
                             , arcBallElevation = 30 * pi / 180
                             }
     camB <- cameraTransB cam events viewportB
     let camNode = (`transNode` item) <$> camB
         clipNode = clipperNode  <$> viewportB <*> camNode
     return clipNode
