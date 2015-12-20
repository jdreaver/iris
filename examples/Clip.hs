-- | Shows how to use a clipper node to split the viewport

module Main where

import qualified Graphics.Rendering.OpenGL as GL

import qualified Iris.Backends.GLFW as W
import           Iris.Camera
import           Iris.Reactive
import           Iris.SceneGraph
import           Iris.Util


main :: IO ()
main =
  do win <- W.makeWindow "Line Plot" (640, 480)
     canvas <- W.initGLFW win

     cube <- makeCube

     let clip1 = clipperNode (Viewport (GL.Position 0 0) (GL.Size 320 480)) [cube]
         clip2 = clipperNode (Viewport (GL.Position 320 0) (GL.Size 320 480)) [cube]
         cam  = arcBallCamera { arcBallWidth     = 6
                              , arcBallAzimuth   = 30 * pi / 180
                              , arcBallElevation = 30 * pi / 180
                              }
         scene = groupNode [clip1, clip2]

     network <- compile $ makeScene canvas (pure scene) (Just cam)
     actuate network

     W.mainLoop canvas
