-- | Example to show off the Axis visual.

module Main where


import qualified Linear as L

import qualified Iris.Backends.GLFW as W
import           Iris.Camera
import           Iris.Reactive
import           Iris.SceneGraph
import           Iris.Visuals


main :: IO ()
main =
  do win <- W.makeWindow "Line Plot" (640, 640)
     canvas <- W.initGLFW win


     let axis = defaultAxis { axisMin = 105
                            , axisMax = 205
                            }
         cam = panZoomCamera { panZoomWidth = 2
                             , panZoomHeight = 2
                             }

     axisItem <- axisInit $ AxisSpec axis (L.V2 (-0.5) (-0.5)) (L.V2 1 0.1)

     network <- compile $ sceneWithCamera canvas (pure axisItem) cam
     actuate network

     W.mainLoop canvas
