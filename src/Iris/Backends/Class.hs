{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Defines the interface that all backends need to conform to.

module Iris.Backends.Class
       ( Canvas (..)
       , CanvasEvents (..)
       , CanvasEventHandler (..)
       , canvasEventHandler
       , mousePosObservable
       , mouseButtonEvent
       , mouseScrollEvent
       , canvasSizeObservable
       , framebufferSizeObservable
       , drawEvent
       , attachEventHandlers
       ) where

import           Control.Lens
import qualified Graphics.Rendering.OpenGL as GL

import           Iris.Events
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


data CanvasEventHandler = CanvasEventHandler
  { mousePosEventHandler    :: Maybe (EventHandler GL.Position)
  , mouseButtonEventHandler :: Maybe (EventHandler MouseButtonEvent)
  , mouseScrollEventHandler :: Maybe (EventHandler GL.GLfloat)
  , canvasSizeEventHandler  :: Maybe (EventHandler GL.Size)
  }

canvasEventHandler :: CanvasEventHandler
canvasEventHandler = CanvasEventHandler Nothing Nothing Nothing Nothing

attachEventHandlers :: CanvasEvents -> CanvasEventHandler -> MomentIO ()
attachEventHandlers es h =
  do maybe (return ()) (\h' -> h' $ es ^. mousePosObservable ^. event) (mousePosEventHandler h)
     maybe (return ()) (\h' -> h' $ es ^. mouseButtonEvent) (mouseButtonEventHandler h)
     maybe (return ()) (\h' -> h' $ es ^. mouseScrollEvent) (mouseScrollEventHandler h)
     maybe (return ()) (\h' -> h' $ es ^. canvasSizeObservable ^. event) (canvasSizeEventHandler h)
