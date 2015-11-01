-- | Defines the interface that all backends need to conform to.

module Iris.Backends.Class
       ( Window (..)
       ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import           Reactive.Banana
import           Reactive.Banana.Frameworks

import qualified Iris.Backends.GLFW as GB
import           Iris.Mouse
import           Iris.Reactive


-- | Used for a common interface for OpenGL windows.
class Window a where

  windowSize :: a -> IO GL.Size
  framebufferSize :: a -> IO GL.Size
  drawLoop :: IO () -> a -> IO ()
  cursorPos :: a -> IO GL.Position

  mousePosObservable :: a -> MomentIO (Observable GL.Position)
  mouseButtonEvent :: a -> MomentIO (Event (MouseButton, MouseButtonState))
  mouseScrollEvent :: a -> MomentIO (Event GL.GLfloat)
  windowSizeObservable :: a -> MomentIO (Observable GL.Size)


instance Window GLFW.Window where
  windowSize = GB.windowSize
  framebufferSize = GB.framebufferSize
  drawLoop = GB.mainLoop
  cursorPos = GB.cursorPos
  mousePosObservable = GB.mousePosObservable
  mouseButtonEvent = GB.mouseButtonEvent
  mouseScrollEvent = GB.mouseScrollEvent
  windowSizeObservable = GB.windowSizeObservable
