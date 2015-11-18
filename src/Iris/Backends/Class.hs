{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Defines the interface that all backends need to conform to.

module Iris.Backends.Class
       ( Window (..)
       , WindowEvents (..)
       , WindowEventHandler (..)
       , windowEventHandler
       , mousePosObservable
       , mouseButtonEvent
       , mouseScrollEvent
       , windowSizeObservable
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
class Window a where

  windowSize      :: a -> IO GL.Size
  framebufferSize :: a -> IO GL.Size
  drawLoop        :: a -> IO ()
  cursorPos       :: a -> IO GL.Position
  makeEvents      :: a -> MomentIO WindowEvents

-- | Data type containing all needed events from a backend Window
data WindowEvents = WindowEvents
  { _windowEventsMousePosObservable   :: Observable GL.Position
  , _windowEventsMouseButtonEvent     :: Event MouseButtonEvent
  , _windowEventsMouseScrollEvent     :: Event GL.GLfloat
  , _windowEventsWindowSizeObservable :: Observable GL.Size
  , _windowEventsDrawEvent            :: Event ()
  }

makeFields ''WindowEvents


data WindowEventHandler = WindowEventHandler
  { mousePosEventHandler    :: Maybe (EventHandler GL.Position)
  , mouseButtonEventHandler :: Maybe (EventHandler MouseButtonEvent)
  , mouseScrollEventHandler :: Maybe (EventHandler GL.GLfloat)
  , windowSizeEventHandler  :: Maybe (EventHandler GL.Size)
  }

windowEventHandler :: WindowEventHandler
windowEventHandler = WindowEventHandler Nothing Nothing Nothing Nothing

attachEventHandlers :: WindowEvents -> [WindowEventHandler] -> MomentIO ()
attachEventHandlers es hs =
  do handleEvent (mapMaybe mousePosEventHandler hs) (es ^. mousePosObservable ^. event)
     handleEvent (mapMaybe mouseButtonEventHandler hs) (es ^. mouseButtonEvent)
     handleEvent (mapMaybe mouseScrollEventHandler hs) (es ^. mouseScrollEvent)
     handleEvent (mapMaybe windowSizeEventHandler hs) (es ^. windowSizeObservable ^. event)
