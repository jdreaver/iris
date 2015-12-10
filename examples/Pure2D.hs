-- | Example using the pure scene graph to make a static scene.

module Main where

import           Control.Monad (unless)
import qualified Graphics.GLUtil as U
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Linear as L

import qualified Iris.Backends.GLFW as W
import           Iris.Camera.PanZoom
import           Iris.DrawGraph
import           Iris.Visuals.Line


main :: IO ()
main =
  do win <- W.makeWindow "Line Plot" (640, 480)
     line <- makeLine

     let lineNode = DrawableNode (drawLine line)
         cam = panZoomCamera { center = L.V2 1 2 , width = 10 , height = 7 }
         camT = panZoomTrans cam
         camGroup = defaultGroupData { transformation = Just camT }
         rootGroup = defaultGroupData { preDrawFunc = drawRoot win }
         scene = GroupNode rootGroup [GroupNode camGroup [lineNode]]

     mainLoop win (drawGraph scene)

mainLoop :: GLFW.Window -> IO () -> IO ()
mainLoop w drawFunc = do
    close <- GLFW.windowShouldClose w
    unless close $ do
                    drawFunc
                    GLFW.swapBuffers w
                    GLFW.pollEvents
                    mainLoop w drawFunc
    W.cleanup w

drawRoot :: GLFW.Window -> IO ()
drawRoot win =
  do (x, y) <- GLFW.getFramebufferSize win
     let winSize =  GL.Size (fromIntegral x) (fromIntegral y)
     GL.viewport $= (GL.Position 0 0, winSize)
     GL.scissor $= Just (GL.Position 0 0, winSize)

     GL.clearColor $= GL.Color4 0 0 0 1
     GL.depthFunc $= Just GL.Less
     GL.clear [GL.ColorBuffer, GL.DepthBuffer]


makeLine :: IO LineItem
makeLine =
  do prog <- U.simpleShaderProgramBS vsSource fsSource
     buff <- U.fromSource GL.ArrayBuffer lineVerts
     return $ LineItem prog buff lineVerts (L.V3 0.2 0.5 1)


lineVerts :: LineVertices
lineVerts = [ L.V2 0 0
            , L.V2 0 1
            , L.V2 1 1
            , L.V2 1 0
            , L.V2 0 0
            ]
