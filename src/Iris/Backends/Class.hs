{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Defines the interface that all backends need to conform to.

module Iris.Backends.Class
       ( Canvas (..)
       , CanvasEvents (..)
       , mousePosObservable
       , mouseButtonEvent
       , mouseScrollEvent
       , canvasSizeObservable
       , framebufferSizeObservable
       , drawEvent
       ) where

import           Control.Lens
import qualified Graphics.Rendering.OpenGL as GL

import           Iris.Mouse
import           Iris.Reactive


-- | Used for a common interface for OpenGL windows.
class Canvas a where

  canvasSize      :: a -> IO GL.Size
  framebufferSize :: a -> IO GL.Size
  drawLoop        :: a -> IO ()
  cursorPos       :: a -> IO GL.Position
  makeEvents      :: a -> MomentIO CanvasEvents

-- | Data type containing all needed events from a backend Canvas
data CanvasEvents = CanvasEvents
  { _canvasEventsMousePosObservable        :: Observable GL.Position
  , _canvasEventsMouseButtonEvent          :: Event MouseButtonEvent
  , _canvasEventsMouseScrollEvent          :: Event GL.GLfloat
  , _canvasEventsCanvasSizeObservable      :: Observable GL.Size
  , _canvasEventsFramebufferSizeObservable :: Observable GL.Size
  , _canvasEventsDrawEvent                 :: Event ()
  }

makeFields ''CanvasEvents
