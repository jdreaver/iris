-- | Defines the interface that all backends need to conform to.

module Iris.Backends.Class
       ( Canvas (..)
       , CanvasEvents (..)
       , CanvasSize (..)
       , FramebufferSize (..)
       , MousePosition (..)
       , MouseScrollAmount (..)
       ) where

import qualified Graphics.Rendering.OpenGL as GL

import           Iris.Mouse
import           Iris.Reactive


-- | Used for a common interface for OpenGL windows.
class Canvas a where

  canvasSize      :: a -> IO CanvasSize
  framebufferSize :: a -> IO FramebufferSize
  drawLoop        :: a -> IO ()
  cursorPos       :: a -> IO MousePosition
  makeEvents      :: a -> MomentIO CanvasEvents

-- The following data types are just wrappers around common OpenGL types,
-- except more specific.
data CanvasSize = CanvasSize !GL.GLsizei !GL.GLsizei
  deriving (Show, Ord, Eq)

data FramebufferSize = FramebufferSize !GL.GLsizei !GL.GLsizei
  deriving (Show, Ord, Eq)

data MousePosition = MousePosition !GL.GLint !GL.GLint
  deriving (Show, Ord, Eq)

newtype MouseScrollAmount = MouseScrollAmount GL.GLfloat
  deriving (Show, Ord, Eq)

-- | Data type containing all needed events from a backend Canvas
data CanvasEvents = CanvasEvents
  { mousePosObservable        :: Observable MousePosition
  , mouseButtonEvent          :: Event MouseButtonEvent
  , mouseScrollEvent          :: Event MouseScrollAmount
  , canvasSizeObservable      :: Observable CanvasSize
  , framebufferSizeObservable :: Observable FramebufferSize
  , drawEvent                 :: Event ()
  }
