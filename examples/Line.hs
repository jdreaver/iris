{-# LANGUAGE MultiWayIf, OverloadedStrings #-}

-- | This is simply a test to make sure things work until I can come up with
-- some real examples.

module Main where

import           Control.Concurrent.STM
import           Control.Lens
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
  do events <- W.makeEvents win

     -- Do we really need to create a new event? We need a recursive definition
     -- of camera state.
     sCam <- tVarSubject camTVar

     bPressedButtons <- recordButtons events (sCam ^. behavior)

     eMove <- dragMove events bPressedButtons sCam
     reactimate eMove

     doScroll <- scroll events sCam
     reactimate doScroll


recordButtons :: (Window a) =>
                 WindowEvents a ->
                 Behavior CameraState ->
                 MomentIO (Behavior PressedButtons)
recordButtons events bCam =
  do let bCamPos = (,) <$> bCam
                       <*> (events ^. mousePosObservable ^. behavior)
         eTagged = (,) <$> bCamPos <@> (events ^. mouseButtonEvent)
         applyClick :: ((CameraState, GL.Position), (MouseButton, MouseButtonState)) ->
                       PressedButtons ->
                       PressedButtons
         applyClick ((s, p), (b, bs)) = recordClick (center s) b bs p
     accumB pressedButtons (applyClick <$> eTagged)

dragMove :: (Window a) =>
            WindowEvents a ->
            Behavior PressedButtons ->
            Subject CameraState ->
            MomentIO (Event (IO ()))
dragMove events bPressedButtons sCam =
  do let bPressedSize = (,,) <$> bPressedButtons
                             <*> events ^. windowSizeObservable ^. behavior
                             <*> sCam ^. behavior
         eDoMove = (,) <$> bPressedSize
                       <@> events ^. mousePosObservable ^. event
         doMove :: ((PressedButtons, GL.Size, CameraState), GL.Position) -> IO ()
         doMove ((pbs, size, cs), pos) =
           do let bs = buttonPressed MouseButtonLeft pbs
              case bs of
                Nothing    -> return ()
                (Just bs') -> (sCam ^. handler) $ mouseDrag size pos bs' cs
     return $ doMove <$> eDoMove

scroll :: (Window a) =>
          WindowEvents a ->
          Subject CameraState ->
          MomentIO (Event (IO ()))
scroll events sCam =
  do let bPosSize = (,,) <$> events ^. windowSizeObservable ^. behavior
                         <*> events ^. mousePosObservable ^. behavior
                         <*> sCam ^. behavior
         eDoScroll = (,) <$> bPosSize
                         <@> events ^. mouseScrollEvent
         doScroll :: ((GL.Size, GL.Position, CameraState), GL.GLfloat) -> IO ()
         doScroll ((size, pos, cs), ds) = (sCam ^. handler) $ mouseZoom size pos ds cs
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
