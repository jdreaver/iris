-- | Module to easily create a GLFW window and clean up when done. Most of this
-- is copied verbatim from <https://github.com/bergey/haskell-OpenGL-examples>.
-- We will probably replace this file once we have an official GLFW backend.


module Iris.Backends.GLFW
       ( makeWindow
       , initGLFW
       , GLFWCanvas (..)
       , mainLoop
       , mainLoop'
       , cleanup
       , module Iris.Backends.Class
       ) where

import           Control.Monad
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import           System.Exit
import           System.IO

import           Iris.Backends.Class
import           Iris.Mouse
import           Iris.Reactive

data GLFWCanvas = GLFWCanvas
  { glfwCanvasWindow        :: GLFW.Window
  , glfwCanvasWindowEvents  :: MomentIO CanvasEvents
  , glfwCanvasFireDraw      :: Handler ()
  }

instance Canvas GLFWCanvas where
  canvasViewport  = windowSize' . glfwCanvasWindow
  framebufferSize = framebufferSize' . glfwCanvasWindow
  drawLoop        = mainLoop
  cursorPos       = cursorPos' . glfwCanvasWindow
  makeEvents      = glfwCanvasWindowEvents

-- | Create a GLFWCanvas from a raw GLFW Window. The only point of this
-- function is to create an event for the draw callback. GLFW doesn't have a
-- general "draw" function that is called in an event loop, so the only way to
-- imitate one is to use a handler and an event in a manual infinite loop.
initGLFW :: GLFW.Window -> IO GLFWCanvas
initGLFW w =
  do (addHandler, fire) <- newAddHandler
     let drawEvent'     = fromAddHandler addHandler
     mousePosO          <- mousePosObservable' w
     mouseButtonE       <- mouseButtonEvent' w
     mouseScrollE       <- mouseScrollEvent' w
     windowSizeO        <- windowSizeObservable' w
     framebufferSizeO   <- framebufferSizeObservable' w
     let events = CanvasEvents <$> mousePosO
                               <*> mouseButtonE
                               <*> mouseScrollE
                               <*> windowSizeO
                               <*> framebufferSizeO
                               <*> drawEvent'
     return $ GLFWCanvas w events fire


-- | Create a GLFW window with the given name and (width, height).
makeWindow :: String -> (Int, Int) -> IO GLFW.Window
makeWindow title (width, height) = do
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
mainLoop :: GLFWCanvas -> IO ()
mainLoop c = mainLoop' (glfwCanvasWindow c) (glfwCanvasFireDraw c ())


mainLoop' :: GLFW.Window -> IO () -> IO ()
mainLoop' w drawFunc = do
    close <- GLFW.windowShouldClose w
    unless close $ do
                    drawFunc
                    GLFW.swapBuffers w
                    GLFW.pollEvents
                    mainLoop' w drawFunc
    cleanup w


cleanup :: GLFW.Window -> IO ()
cleanup win = do
    GLFW.destroyWindow win
    GLFW.terminate
    exitSuccess

windowSize' :: GLFW.Window -> IO Viewport
windowSize' win =
  do (w, h) <- GLFW.getWindowSize win
     return $ Viewport (GL.Position 0 0) (GL.Size (fromIntegral w) (fromIntegral h))

framebufferSize' :: GLFW.Window -> IO FramebufferSize
framebufferSize' win =
  do (x, y) <- GLFW.getFramebufferSize win
     return $ FramebufferSize (fromIntegral x) (fromIntegral y)

-- | Overload of `GLFW.getCursorPos` to return `GL.Position`
cursorPos' :: GLFW.Window -> IO MousePosition
cursorPos' w =
  do (x, y) <- GLFW.getCursorPos w
     return $ MousePosition (floor x) (floor y)


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
mousePosObservable' :: GLFW.Window -> IO (MomentIO (Observable MousePosition))
mousePosObservable' win =
  do (addHandler, fire) <- newAddHandler
     let callback :: GLFW.CursorPosCallback
         callback _ x y = fire pos
           where pos = MousePosition (floor x) (floor y)
     liftIO $ GLFW.setCursorPosCallback win $ Just callback
     currentPos <- cursorPos' win
     return $ do e <- fromAddHandler addHandler
                 b <- stepper currentPos e
                 return $ Observable b e

-- | Create an Event for mouse button presses/releases using the GLFW mouse
-- button callback.
mouseButtonEvent' :: GLFW.Window -> IO (MomentIO (Event MouseButtonEvent))
mouseButtonEvent' win =
  do (addHandler, fire) <- newAddHandler
     let callback :: GLFW.MouseButtonCallback
         callback _ b s _ =
           do let mbutton = mouseButton b
                  state   = mouseButtonState s
              case mbutton of
                Nothing       -> return ()
                (Just button) -> fire (button, state)
     GLFW.setMouseButtonCallback win $ Just callback
     return $ fromAddHandler addHandler

-- | Create an Event for mouse button scrolling using the GLFW scroll callback.
mouseScrollEvent' :: GLFW.Window -> IO (MomentIO (Event MouseScrollAmount))
mouseScrollEvent' win =
  do (addHandler, fire) <- newAddHandler
     let callback :: GLFW.ScrollCallback
         callback _ _ ds = fire (MouseScrollAmount $ realToFrac ds)
     liftIO $ GLFW.setScrollCallback win $ Just callback
     return $ fromAddHandler addHandler

-- | Create an Observable for the window size using the GLFW window size
-- callback.
windowSizeObservable' :: GLFW.Window -> IO (MomentIO (Observable Viewport))
windowSizeObservable' win =
  do currentSize <- windowSize' win
     (h, sub) <- subjectIO currentSize
     let callback :: GLFW.WindowSizeCallback
         callback _ x y = h $ Viewport (GL.Position 0 0) size
           where size = GL.Size (fromIntegral x) (fromIntegral y)
     liftIO $ GLFW.setWindowSizeCallback win $ Just callback
     return $ liftM asObservable sub


-- | Create an Observable for the framebuffer size using the GLFW framebuffer size
-- callback.
framebufferSizeObservable' :: GLFW.Window -> IO (MomentIO (Observable FramebufferSize))
framebufferSizeObservable' win =
  do currentSize <- framebufferSize' win
     (h, sub) <- subjectIO currentSize
     let callback :: GLFW.FramebufferSizeCallback
         callback _ x y = h $ FramebufferSize (fromIntegral x) (fromIntegral y)
     liftIO $ GLFW.setFramebufferSizeCallback win $ Just callback
     return $ liftM asObservable sub
