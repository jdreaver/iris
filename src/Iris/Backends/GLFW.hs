-- | Module to easily create a GLFW window and clean up when done. Most of this
-- is copied verbatim from <https://github.com/bergey/haskell-OpenGL-examples>.
-- We will probably replace this file once we have an official GLFW backend.


module Iris.Backends.GLFW
       ( initGLFW
       , windowSize
       , framebufferSize
       , mainLoop
       , cursorPos
       , mousePosObservable
       , mouseButtonEvent
       , mouseScrollEvent
       , windowSizeObservable
       ) where

import           Control.Monad
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           System.Exit
import           System.IO

import           Iris.Mouse
import           Iris.Reactive

-- | Create a GLFW window with the given name and (width, height).
initGLFW :: String -> (Int, Int) -> IO GLFW.Window
initGLFW title (width, height) = do
  GLFW.setErrorCallback (Just errorCallback)
  successfulInit <- GLFW.init
  -- if init failed, we exit the program
  if not successfulInit then exitFailure else do
      GLFW.windowHint $ GLFW.WindowHint'OpenGLDebugContext True
      GLFW.windowHint $ GLFW.WindowHint'DepthBits 16
      mw <- GLFW.createWindow width height title Nothing Nothing
      case mw of
          Nothing -> GLFW.terminate >> exitFailure
          Just window -> do
              GLFW.makeContextCurrent mw
              GLFW.setKeyCallback window (Just keyCallback)
              return window


-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: GLFW.ErrorCallback
errorCallback _ = hPutStrLn stderr

keyCallback :: GLFW.KeyCallback
keyCallback window key _ action _ =
  when (key == GLFW.Key'Escape && action == GLFW.KeyState'Pressed) $
    GLFW.setWindowShouldClose window True


-- | Runs the given drawing function in GLFW's main loop, and cleans up the
-- window when the user exits.
mainLoop :: IO () -> GLFW.Window -> IO ()
mainLoop draw w = do
    close <- GLFW.windowShouldClose w
    unless close $ do
                    draw
                    GLFW.swapBuffers w
                    GLFW.pollEvents
                    mainLoop draw w
    cleanup w


cleanup :: GLFW.Window -> IO ()
cleanup win = do
    GLFW.destroyWindow win
    GLFW.terminate
    exitSuccess

-- | Overload of `GLFW.getWindowSize` to return `GL.Size`
windowSize :: GLFW.Window -> IO GL.Size
windowSize win =
  do (w, h) <- GLFW.getWindowSize win
     return $ GL.Size (fromIntegral w) (fromIntegral h)

framebufferSize :: GLFW.Window -> IO GL.Size
framebufferSize win =
  do (x, y) <- GLFW.getFramebufferSize win
     return $ GL.Size (fromIntegral x) (fromIntegral y)

-- | Overload of `GLFW.getCursorPos` to return `GL.Position`
cursorPos :: GLFW.Window -> IO GL.Position
cursorPos win =
  do (x, y) <- GLFW.getCursorPos win
     return $ GL.Position (floor x) (floor y)


-- | Convert from GLFW mouse buttons to iris mouse buttons
mouseButton :: GLFW.MouseButton -> Maybe MouseButton
mouseButton GLFW.MouseButton'1 = Just MouseButtonLeft
mouseButton GLFW.MouseButton'2 = Just MouseButtonRight
mouseButton GLFW.MouseButton'3 = Just MouseButtonMiddle
mouseButton _                  = Nothing

-- | Convert from GLFW MouseButtonState to MouseButtonState
mouseButtonState :: GLFW.MouseButtonState -> MouseButtonState
mouseButtonState GLFW.MouseButtonState'Pressed = Pressed
mouseButtonState GLFW.MouseButtonState'Released = Released

-- | Create an Observable for the mouse position using the GLFW mouse
-- position callback.
mousePosObservable :: GLFW.Window -> MomentIO (Observable GL.Position)
mousePosObservable win =
  do (e, h) <- newEvent
     let callback :: GLFW.CursorPosCallback
         callback _ x y = h pos
           where pos = GL.Position (floor x) (floor y)
     liftIO $ GLFW.setCursorPosCallback win $ Just callback
     currentPos <- liftIO $ cursorPos win
     b <- stepper currentPos e
     return $ Observable b e

-- | Create an Event for mouse button presses/releases using the GLFW mouse
-- button callback.
mouseButtonEvent :: GLFW.Window -> MomentIO (Event MouseButtonEvent)
mouseButtonEvent win =
  do (e, h) <- newEvent
     let callback :: GLFW.MouseButtonCallback
         callback _ b s _ =
           do let mbutton = mouseButton b
                  state   = mouseButtonState s
              case mbutton of
                Nothing       -> return ()
                (Just button) -> h (button, state)
     liftIO $ GLFW.setMouseButtonCallback win $ Just callback
     return e

-- | Create an Event for mouse button scrolling using the GLFW scroll callback.
mouseScrollEvent :: GLFW.Window -> MomentIO (Event GL.GLfloat)
mouseScrollEvent win =
  do (e, h) <- newEvent
     let callback :: GLFW.ScrollCallback
         callback _ _ ds = h (realToFrac ds)
     liftIO $ GLFW.setScrollCallback win $ Just callback
     return e

-- | Create an Observable for the window size using the GLFW window size
-- callback.
windowSizeObservable :: GLFW.Window -> MomentIO (Observable GL.Size)
windowSizeObservable win =
  do (e, h) <- newEvent
     let callback :: GLFW.WindowSizeCallback
         callback _ x y = h $ GL.Size (fromIntegral x) (fromIntegral y)
     liftIO $ GLFW.setWindowSizeCallback win $ Just callback
     currentSize <- liftIO $ windowSize win
     b <- stepper currentSize e
     return $ Observable b e