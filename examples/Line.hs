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
import qualified Iris.Util.GLFW as W

main :: IO ()
main =
  do win <- W.initialize "Line Plot" (640, 480)

     cameraState <- newTVarIO $ CameraState (L.V2 1 2) 10 7
     buttonState <- newTVarIO pressedButtons
     GLFW.setMouseButtonCallback win $ Just (mouseButtonCallback cameraState buttonState)
     GLFW.setCursorPosCallback win $ Just (cursorPosCallback cameraState buttonState)
     GLFW.setScrollCallback win $ Just (mouseScrollCallback cameraState)

     lineProg <- initLine lineVerts
     W.mainLoop (draw cameraState lineProg win) win


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
            atomically $ modifyTVar' camTVar (drag size pos bstate)


mouseScrollCallback :: TVar CameraState -> GLFW.ScrollCallback
mouseScrollCallback cam win _ ds =
  do state <- readTVarIO cam
     (xpx, xpy) <- GLFW.getCursorPos win
     (wp, hp) <- GLFW.getWindowSize win
     let f = 1 - 0.1 * ds  -- Zoom factor
         w = realToFrac $ width state
         h = realToFrac $ height state
         w' = w * f
         h' = h * f

         -- Translate center
         (x', y') = mapToWorld (xpx, xpy) (wp, hp) state
         x = L.V2 (realToFrac x') (realToFrac y')
         dx = x - c
         dx' = dx * realToFrac f
         c = center state
         c' = x - dx'

         newState = state { center = c'
                          , width = realToFrac w'
                          , height = realToFrac h' }

     atomically $ writeTVar cam newState

draw :: TVar CameraState -> LineProgram -> GLFW.Window -> IO ()
draw camMVar lp win =
  do GL.clearColor $= GL.Color4 0 0 0 1
     GL.depthFunc $= Just GL.Less
     GL.clear [GL.ColorBuffer, GL.DepthBuffer]

     -- In C++ example GLUT handles this?
     (winWidth, winHeight) <- GLFW.getFramebufferSize win
     GL.viewport $= (GL.Position 0 0,
                     GL.Size (fromIntegral winWidth) (fromIntegral winHeight))

     camState <- readTVarIO camMVar
     let m  = transformM camState
     drawLine lp m

mapToWorld :: (Double, Double) -> (Int, Int) -> CameraState -> (Double, Double)
mapToWorld (xp, yp) (w, h) (CameraState (L.V2 cx cy) cw ch) = (x, y)
  where (w', h')   = (fromIntegral w, fromIntegral h)
        (cxp, cyp) = (w' / 2, h' / 2)  -- Camera center in pixels
        (dxp, dyp) = (xp - cxp, yp - cyp)
        (cw', ch') = (fromRational $ toRational cw, fromRational $ toRational ch)
        (cx', cy') = (fromRational $ toRational cx, fromRational $ toRational cy)
        (dx, dy)   = (dxp * cw' / w', dyp * ch' / h' * (-1))
        (x, y)     = (cx' + dx, cy' + dy) :: (Double, Double)


transformM :: CameraState -> L.M44 GL.GLfloat
transformM (CameraState (L.V2 cx cy) w h) = scale L.!*! trans L.!*! model where
  model = L.identity
  trans = L.V4 (L.V4 1 0 0 (-cx)) (L.V4 0 1 0 (-cy)) (L.V4 0 0 1 0) (L.V4 0 0 0 1)
  scale = L.V4 (L.V4 (2/w) 0 0 0) (L.V4 0 (2/h) 0 0) (L.V4 0 0 1 0) (L.V4 0 0 0 1)

lineVerts :: LineVertices
lineVerts = [ L.V2 1 1
            , L.V2 1 2
            , L.V2 2 2
            ]
