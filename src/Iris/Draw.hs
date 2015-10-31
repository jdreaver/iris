-- | Functions for drawing plot items

module Iris.Draw
       ( PlotItem(..)
       , MVPTransform
       , draw
       ) where

import           Control.Monad (forM_)
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L

import Iris.Camera

-- | Wrapper around an individual item to plot.
data PlotItem = PlotItem
  { drawFunc :: MVPTransform -> IO ()
  }


-- | Draw a list of plot items given the camera state.
draw :: CameraState -> [PlotItem] -> IO ()
draw camState items =
  do GL.clearColor $= GL.Color4 0 0 0 1
     GL.depthFunc $= Just GL.Less
     GL.clear [GL.ColorBuffer, GL.DepthBuffer]

     let m  = transformM camState
     forM_ items (`drawFunc` m)


-- | Transformation matrix applied in vertex shader for each plot item.
type MVPTransform = L.M44 GL.GLfloat

-- | Creates the MVP transformation matrix given the camera state.
transformM :: CameraState -> MVPTransform
transformM (CameraState (L.V2 cx cy) w h) = scale L.!*! trans L.!*! model where
  model = L.identity
  trans = L.V4 (L.V4 1 0 0 (-cx)) (L.V4 0 1 0 (-cy)) (L.V4 0 0 1 0) (L.V4 0 0 0 1)
  scale = L.V4 (L.V4 (2/w) 0 0 0) (L.V4 0 (2/h) 0 0) (L.V4 0 0 1 0) (L.V4 0 0 0 1)
