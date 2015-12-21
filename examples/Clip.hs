-- | Shows how to use a clipper node to split the viewport

module Main where

import           Control.Lens
import qualified Graphics.Rendering.OpenGL as GL

import           Iris.Backends
import qualified Iris.Backends.GLFW as W
import           Iris.Camera
import           Iris.Reactive
import           Iris.SceneGraph
import           Iris.Util


main :: IO ()
main =
  do win <- W.makeWindow "Line Plot" (640, 480)
     canvas <- W.initGLFW win

     network <- compile $ makeNetwork canvas
     actuate network

     W.mainLoop canvas


makeNetwork :: (Canvas a) => a -> MomentIO ()
makeNetwork can =
  do (viewB1, viewB2) <- clipBehaviors can
     cube <- liftIO makeCube
     let clip1 = flip clipperNode [cube] <$> viewB1
         clip2 = flip clipperNode [cube] <$> viewB2
         cam  = arcBallCamera { arcBallWidth     = 6
                              , arcBallAzimuth   = 30 * pi / 180
                              , arcBallElevation = 30 * pi / 180
                              }
         scene = groupNode <$> sequenceA [clip1, clip2]
     makeScene can scene (Just cam)


-- | Create a Behavior for the viewport sizes. Each viewport takes up half of
-- the screen.
clipBehaviors :: (Canvas a) => a -> MomentIO (Behavior Viewport, Behavior Viewport)
clipBehaviors c =
  do events <- makeEvents c
     let buffSizeB = events ^. framebufferSizeObservable ^. behavior
         f1 (GL.Size w h) = Viewport (GL.Position 0 0) (GL.Size (w `quot` 2) h)
         f2 (GL.Size w h) = Viewport (GL.Position (w `quot` 2) 0) (GL.Size (w `quot` 2) h)
     return (f1 <$> buffSizeB, f2 <$> buffSizeB)
