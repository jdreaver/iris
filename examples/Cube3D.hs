-- | Example of a simple 3D cube and an arcball camera. The cube vertexes and
-- colors were taken from
-- https://en.wikibooks.org/wiki/OpenGL_Programming/Modern_OpenGL_Tutorial_05

module Main where

import qualified Iris.Backends.GLFW as W
import           Iris.Camera
import           Iris.Reactive
import           Iris.SceneGraph
import           Iris.Util

main :: IO ()
main =
  do win <- W.makeWindow "3D Cube" (640, 640)
     canvas <- W.initGLFW win

     cube <- makeCube

     let cam  = arcBallCamera { arcBallWidth     = 6
                              , arcBallAzimuth   = 30 * pi / 180
                              , arcBallElevation = 30 * pi / 180
                              }

     network <- compile $ sceneWithCamera canvas (pure cube) cam
     actuate network

     W.mainLoop canvas
