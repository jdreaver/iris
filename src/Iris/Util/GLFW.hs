-- | Module to easily create a GLFW window and clean up when done. Most of this
-- is copied verbatim from <https://github.com/bergey/haskell-OpenGL-examples>.
-- We will probably replace this file once we have an official GLFW backend.


module Iris.Util.GLFW
       ( cursorPos
       , initialize
       , mainLoop
       , mouseButton
       , mouseButtonState
       , windowSize
       ) where

import           Control.Monad
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import           System.Exit
import           System.IO

import           Iris.Mouse

-- | Create a GLFW window with the given name and (width, height).
initialize :: String -> (Int, Int) -> IO GLFW.Window
initialize title (width, height) = do
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
