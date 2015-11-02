{-# LANGUAGE MultiWayIf, OverloadedStrings #-}

-- | This is simply a test to make sure things work until I can come up with
-- some real examples.

module Main where

import           Control.Concurrent.STM
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Linear as L
import           Reactive.Banana
import           Reactive.Banana.Frameworks

import           Iris.Backends
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


mouseNetwork :: (Window a) => TVar CameraState -> a -> MomentIO ()
mouseNetwork camTVar win =
  do -- Create events for the various window callbacks
     (Observable bCursorPos eCursorPos) <- W.mousePosObservable win
     (Observable bWinSize _)     <- W.windowSizeObservable win
     eButton <- W.mouseButtonEvent win
     eScroll <- W.mouseScrollEvent win

     -- Do we really need to create a new event? We need a recursive definition
     -- of camera state.
     (Subject bCam _ hCam) <- tVarSubject camTVar

     bPressedButtons <- recordButtons bCam bCursorPos eButton

     eMove <- dragMove bPressedButtons bWinSize bCam eCursorPos hCam
     reactimate eMove

     doScroll <- scroll bWinSize bCursorPos bCam eScroll hCam
     reactimate doScroll


recordButtons :: Behavior CameraState ->
                 Behavior GL.Position ->
                 Event (MouseButton, MouseButtonState) ->
                 MomentIO (Behavior PressedButtons)
recordButtons bCam bCursorPos eButton =
  do let bCamPos = liftA2 (,) bCam bCursorPos
         eTagged = (,) <$> bCamPos <@> eButton
         applyClick :: ((CameraState, GL.Position), (MouseButton, MouseButtonState)) ->
                       PressedButtons ->
                       PressedButtons
         applyClick ((s, p), (b, bs)) = recordClick (center s) b bs p
     accumB pressedButtons (applyClick <$> eTagged)

dragMove :: Behavior PressedButtons ->
            Behavior GL.Size ->
            Behavior CameraState ->
            Event GL.Position ->
            Handler CameraState ->
            MomentIO (Event (IO ()))
dragMove bPressedButtons bWinSize bCam eCursorPos hCam =
  do let bPressedSize = (,,) <$> bPressedButtons <*> bWinSize <*> bCam
         eDoMove = (,) <$> bPressedSize <@> eCursorPos
         doMove :: ((PressedButtons, GL.Size, CameraState), GL.Position) -> IO ()
         doMove ((pbs, size, cs), pos) =
           do let bs = buttonPressed MouseButtonLeft pbs
              case bs of
                Nothing    -> return ()
                (Just bs') -> hCam $ mouseDrag size pos bs' cs
     return $ doMove <$> eDoMove

scroll :: Behavior GL.Size ->
          Behavior GL.Position ->
          Behavior CameraState ->
          Event GL.GLfloat ->
          Handler CameraState ->
          MomentIO (Event (IO ()))
scroll bWinSize bCursorPos bCam eScroll hCam =
  do let bPosSize = (,,) <$> bWinSize <*> bCursorPos <*> bCam
         eDoScroll = (,) <$> bPosSize <@> eScroll
         doScroll :: ((GL.Size, GL.Position, CameraState), GL.GLfloat) -> IO ()
         doScroll ((size, pos, cs), ds) = hCam $ mouseZoom size pos ds cs
     return $ doScroll <$> eDoScroll



draw' :: TVar CameraState -> SceneNode -> GLFW.Window -> IO ()
draw' camMVar root win =
  do cam <- readTVarIO camMVar
     drawScene win (Scene root cam)

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
