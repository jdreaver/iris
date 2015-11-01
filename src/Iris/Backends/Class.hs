-- | Defines the interface that all backends need to conform to.

module Iris.Backends.Class
       ( Window (..)
       ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import           Reactive.Banana
import           Reactive.Banana.Frameworks

import           Iris.Mouse
import qualified Iris.Backends.GLFW as GB


-- | Used for a common interface for OpenGL windows.
class Window a where

  windowSize :: a -> IO GL.Size
  framebufferSize :: a -> IO GL.Size
  drawLoop :: IO () -> a -> IO ()
  cursorPos :: a -> IO GL.Position

  mousePosEvent :: a -> MomentIO (Event GL.Position)
  mouseButtonEvent :: a -> MomentIO (Event (MouseButton, MouseButtonState))
  mouseScrollEvent :: a -> MomentIO (Event GL.GLfloat)
  windowSizeEvent :: a -> MomentIO (Event GL.Size)


instance Window GLFW.Window where
  windowSize = GB.windowSize'
  framebufferSize = GB.framebufferSize
  drawLoop = GB.mainLoop
  cursorPos = GB.cursorPos'
  mousePosEvent = GB.mousePosEvent'
  mouseButtonEvent = GB.mouseButtonEvent'
  mouseScrollEvent = GB.mouseScrollEvent'
  windowSizeEvent = GB.windowSizeEvent'
