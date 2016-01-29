{-# LANGUAGE CPP #-}

-- | Shows how to use a clipper node to split the viewport, and how to use
-- different different cameras in the same scene. There are 3 arcball cameras,
-- and one pan-zoom camera (the top-right camera), all using the same cube.

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
     clip4 <- panZoomNode viewB4 events cube
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

arcBallNode :: Behavior Viewport -- ^ Viewport for this scene
            -> CanvasEvents
            -> DrawNode          -- ^ Thing to draw
            -> MomentIO (Behavior DrawNode)
arcBallNode viewportB
            (CanvasEvents mousePosO mouseButtonE mouseScrollE _ _ _)
            item =
  do let cam = arcBallCamera { arcBallWidth     = 6
                             , arcBallAzimuth   = 30 * pi / 180
                             , arcBallElevation = 30 * pi / 180
                             }
     camB <- arcBallTransB cam mousePosO mouseButtonE viewportB mouseScrollE
     return $ makeNode camB viewportB item


panZoomNode :: Behavior Viewport -- ^ Viewport for this scene
            -> CanvasEvents
            -> DrawNode          -- ^ Thing to draw
            -> MomentIO (Behavior DrawNode)
panZoomNode viewportB
            (CanvasEvents mousePosO mouseButtonE mouseScrollE _ _ _)
            item =
  do let cam = panZoomCamera { panZoomWidth  = 6
                             , panZoomHeight = 6
                             }
     camB <- panZoomTransB cam mousePosO mouseButtonE viewportB mouseScrollE
     return $ makeNode camB viewportB item


makeNode :: Behavior Transformation
         -> Behavior Viewport
         -> DrawNode
         -> Behavior DrawNode
makeNode camB viewportB item = clipperNode <$> viewportB <*> ((`transNode` item) <$> camB)
