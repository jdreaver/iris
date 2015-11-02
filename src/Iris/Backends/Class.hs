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
  makeEvents :: a -> MomentIO (WindowEvents a)


instance Window GLFW.Window where
  windowSize = GB.windowSize
  framebufferSize = GB.framebufferSize
  drawLoop = GB.mainLoop
  cursorPos = GB.cursorPos
  makeEvents w = WindowEvents <$> GB.mousePosObservable w
                              <*> GB.mouseButtonEvent w
                              <*> GB.mouseScrollEvent w
                              <*> GB.windowSizeObservable w

-- | Data type containing all needed events from a backend Window
data WindowEvents a = WindowEvents
  { _windowEventsMousePosObservable   :: Observable GL.Position
  , _windowEventsMouseButtonEvent     :: Event MouseButtonEvent
  , _windowEventsMouseScrollEvent     :: Event GL.GLfloat
  , _windowEventsWindowSizeObservable :: Observable GL.Size
  }

makeFields ''WindowEvents
