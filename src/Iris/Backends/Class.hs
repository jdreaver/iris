{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Defines the interface that all backends need to conform to.

module Iris.Backends.Class
       ( Window (..)
       , WindowEvents (..)
       , mousePosObservable
       , mouseButtonEvent
       , mouseScrollEvent
       , windowSizeObservable
       ) where

import           Control.Lens
import qualified Graphics.Rendering.OpenGL as GL
import           Reactive.Banana
import           Reactive.Banana.Frameworks

import           Iris.Mouse
import           Iris.Reactive


-- | Used for a common interface for OpenGL windows.
class Window a where

  windowSize :: a -> IO GL.Size
  framebufferSize :: a -> IO GL.Size
  drawLoop :: IO () -> a -> IO ()
  cursorPos :: a -> IO GL.Position
  makeEvents :: a -> MomentIO WindowEvents

-- | Data type containing all needed events from a backend Window
data WindowEvents = WindowEvents
  { _windowEventsMousePosObservable   :: Observable GL.Position
  , _windowEventsMouseButtonEvent     :: Event MouseButtonEvent
  , _windowEventsMouseScrollEvent     :: Event GL.GLfloat
  , _windowEventsWindowSizeObservable :: Observable GL.Size
  }

makeFields ''WindowEvents
