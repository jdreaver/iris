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
       , drawEvent
       , attachEventHandlers
       ) where

import           Control.Lens
import           Data.Maybe (mapMaybe)
import qualified Graphics.Rendering.OpenGL as GL
import           Reactive.Banana
import           Reactive.Banana.Frameworks

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
  { _canvasEventsMousePosObservable   :: Observable GL.Position
  , _canvasEventsMouseButtonEvent     :: Event MouseButtonEvent
  , _canvasEventsMouseScrollEvent     :: Event GL.GLfloat
  , _canvasEventsCanvasSizeObservable :: Observable GL.Size
  , _canvasEventsDrawEvent            :: Event ()
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

attachEventHandlers :: CanvasEvents -> [CanvasEventHandler] -> MomentIO ()
attachEventHandlers es hs =
  do handleEvent (mapMaybe mousePosEventHandler hs)    (es ^. mousePosObservable ^. event)
     handleEvent (mapMaybe mouseButtonEventHandler hs) (es ^. mouseButtonEvent)
     handleEvent (mapMaybe mouseScrollEventHandler hs) (es ^. mouseScrollEvent)
     handleEvent (mapMaybe canvasSizeEventHandler hs)  (es ^. canvasSizeObservable ^. event)
