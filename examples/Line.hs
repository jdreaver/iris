{-# LANGUAGE MultiWayIf, OverloadedStrings #-}

-- | This is simply a test to make sure things work until I can come up with
-- some real examples.

module Main where

import           Control.Concurrent.STM
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Linear as L

import           Iris.Camera
import           Iris.Line
import           Iris.Mouse
import           Iris.SceneGraph
import           Iris.Transformation
import           Iris.Triangle
import qualified Iris.Util.GLFW as W

main :: IO ()
main =
  do win <- W.initialize "Line Plot" (640, 480)

     cameraState <- newTVarIO $ CameraState (L.V2 1 2) 10 7
     buttonState <- newTVarIO pressedButtons
     GLFW.setMouseButtonCallback win $ Just (mouseButtonCallback cameraState buttonState)
     GLFW.setCursorPosCallback win $ Just (cursorPosCallback cameraState buttonState)
     GLFW.setScrollCallback win $ Just (mouseScrollCallback cameraState)

     line <- lineItem lineVerts (L.V3 0.2 0.5 1)
     tri <- triangleItem triVerts (L.V3 0.2 1 0.1)

     let root = Collection [ Drawable line
                           , Drawable tri
                           , Transform (translation (L.V3 (-1) 1 0)) (Drawable tri)]

     W.mainLoop (draw' cameraState root win) win


mouseButtonCallback :: TVar CameraState -> TVar PressedButtons -> GLFW.MouseButtonCallback
mouseButtonCallback camTVar mouseTVar win button state  _ =
  do let mbutton = W.mouseButton button
     case mbutton of
       Nothing        -> return ()
       (Just button') ->
         do cameraState <- readTVarIO camTVar
            pos <- W.cursorPos win
            let state'    = W.mouseButtonState state
                camCenter = center cameraState
                f         = recordClick camCenter button' state' pos
            atomically $ modifyTVar' mouseTVar f


cursorPosCallback :: TVar CameraState -> TVar PressedButtons -> GLFW.CursorPosCallback
cursorPosCallback camTVar mouseTVar win x y =
  do
     buttonState <- readTVarIO mouseTVar
     let leftButtonState = buttonPressed MouseButtonLeft buttonState
     case leftButtonState of
       Nothing       -> return ()
       (Just bstate) ->
         do size <- W.windowSize win
            let pos = GL.Position (floor x) (floor y)
            atomically $ modifyTVar' camTVar (mouseDrag size pos bstate)


mouseScrollCallback :: TVar CameraState -> GLFW.ScrollCallback
mouseScrollCallback camTVar win _ ds =
  do size  <- W.windowSize win
     pos   <- W.cursorPos win
     atomically $ modifyTVar' camTVar (mouseZoom size pos ds)

draw' :: TVar CameraState -> SceneNode -> GLFW.Window -> IO ()
draw' camMVar root win =
  do -- In C++ example GLUT handles this?
     (winWidth, winHeight) <- GLFW.getFramebufferSize win
     GL.viewport $= (GL.Position 0 0,
                     GL.Size (fromIntegral winWidth) (fromIntegral winHeight))

     cam <- readTVarIO camMVar
     let scene = Scene root cam

     drawScene scene

lineVerts :: LineVertices
lineVerts = [ L.V2 1 1
            , L.V2 1 2
            , L.V2 2 2
            ]

triVerts :: TriangleVertices
triVerts = [ L.V2 0 0
           , L.V2 0 1
           , L.V2 1 0
           ]
