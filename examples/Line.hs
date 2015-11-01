{-# LANGUAGE MultiWayIf, OverloadedStrings #-}

-- | This is simply a test to make sure things work until I can come up with
-- some real examples.

module Main where

import           Control.Concurrent.STM
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Linear as L
import           Reactive.Banana
import           Reactive.Banana.Frameworks

import qualified Iris.Backends as W
import           Iris.Camera
import           Iris.Line
import           Iris.Mouse
import           Iris.Reactive
import           Iris.SceneGraph
import           Iris.Transformation
import           Iris.Triangle

main :: IO ()
main =
  do win <- W.initGLFW "Line Plot" (640, 480)

     cameraState <- newTVarIO $ CameraState (L.V2 1 2) 10 7

     line <- lineItem lineVerts (L.V3 0.2 0.5 1)
     tri <- triangleItem triVerts (L.V3 0.2 1 0.1)

     network <- compile $ mouseNetwork cameraState win
     actuate network

     let root = Collection [ Drawable line
                           , Drawable tri
                           , Transform (translation (L.V3 (-1) 1 0)) (Drawable tri)]

     W.drawLoop (draw' cameraState root win) win


mouseNetwork :: TVar CameraState -> GLFW.Window -> MomentIO ()
mouseNetwork camTVar win =
  do -- Create events for the various window callbacks
     eCursorPos <- W.mousePosEvent win
     eButton    <- W.mouseButtonEvent win
     eWinSize   <- W.windowSizeEvent win
     eScroll    <- W.mouseScrollEvent win

     -- Create Behaviors for cursor position and window size, since they are
     -- sampled when we do mouse clicks.
     bCursorPos  <- stepper (GL.Position 0 0) eCursorPos
     currentSize <- liftIO $ W.windowSize win
     bWinSize    <- stepper currentSize eWinSize

     -- Do we really need to create a new event? We need a recursive definition
     -- of camera state.
     (bCam, _, hCam) <- tVarBehavior camTVar

     -- Create a Behavior for the currently pressed buttons and some state from
     -- when the buttons were pressed. This state is needed to implement
     -- dragging.
     let bCamPos = liftA2 (,) bCam bCursorPos
         eTagged = (,) <$> bCamPos <@> eButton
         applyClick :: ((CameraState, GL.Position), (MouseButton, MouseButtonState)) ->
                       PressedButtons ->
                       PressedButtons
         applyClick ((s, p), (b, bs)) = recordClick (center s) b bs p
     bPressedButtons <- accumB pressedButtons (applyClick <$> eTagged)

     -- Create the event and action for when the user drags the mouse and the
     -- left mouse button is pressed.
     let bPressedSize = (,,) <$> bPressedButtons <*> bWinSize <*> bCam
         eDoMove = (,) <$> bPressedSize <@> eCursorPos
         doMove :: ((PressedButtons, GL.Size, CameraState), GL.Position) -> IO ()
         doMove ((pbs, size, cs), pos) =
           do let bs = buttonPressed MouseButtonLeft pbs
              case bs of
                Nothing    -> return ()
                (Just bs') -> hCam $ mouseDrag size pos bs' cs
     reactimate $ doMove <$> eDoMove

     -- Create the event and action for scrolling
     let bPosSize = (,,) <$> bWinSize <*> bCursorPos <*> bCam
         eDoScroll = (,) <$> bPosSize <@> eScroll
         doScroll :: ((GL.Size, GL.Position, CameraState), GL.GLfloat) -> IO ()
         doScroll ((size, pos, cs), ds) = hCam $ mouseZoom size pos ds cs
     reactimate $ doScroll <$> eDoScroll


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
