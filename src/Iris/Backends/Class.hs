-- | Defines the interface that all backends need to conform to.

module Iris.Backends.Class
       ( Canvas (..)
       , CanvasEvents (..)
       , FramebufferSize (..)
       , MousePosition (..)
       , MouseScrollAmount (..)
       ) where

import qualified Graphics.Rendering.OpenGL as GL

import           Iris.Mouse
import           Iris.OpenGL (Viewport (..))
import           Iris.Reactive


-- | Used for a common interface for OpenGL windows.
class Canvas a where

  canvasViewport  :: a -> IO Viewport
  framebufferSize :: a -> IO FramebufferSize
  drawLoop        :: a -> IO ()
  cursorPos       :: a -> IO MousePosition
  makeEvents      :: a -> MomentIO CanvasEvents

-- The following data types are just wrappers around common OpenGL types,
-- except more specific.
data FramebufferSize = FramebufferSize !GL.GLsizei !GL.GLsizei
  deriving (Show, Ord, Eq)

-- | Used in events to relay the current mouse position in integer pixel
-- coordinates. Note that the origin is assumed to be the lower-left corner!
data MousePosition = MousePosition !GL.GLint !GL.GLint
  deriving (Show, Ord, Eq)

newtype MouseScrollAmount = MouseScrollAmount GL.GLfloat
  deriving (Show, Ord, Eq)

-- | Data type containing all needed events from a backend Canvas
data CanvasEvents = CanvasEvents
  { mousePosObservable        :: Observable MousePosition
  , mouseButtonEvent          :: Event MouseButtonEvent
  , mouseScrollEvent          :: Event MouseScrollAmount
  , viewportObservable        :: Observable Viewport
  , framebufferSizeObservable :: Observable FramebufferSize
  , drawEvent                 :: Event ()
  }
