-- | Base module for all visuals

module Iris.Visuals.Visual
       ( bufferObservable
       , drawVisual
       ) where


import qualified Graphics.GLUtil as U
import qualified Graphics.Rendering.OpenGL as GL

import Iris.Reactive
import Iris.Transformation


-- | Hooks up a visual's draw function to a draw event and a transformation
-- behavior.
drawVisual :: Behavior a ->
              (a -> Transformation -> IO ()) ->
              Event () ->
              Behavior Transformation ->
              MomentIO ()
drawVisual bItem drawFunc eDraw bTrans =
  do let bData = (,) <$> bItem <*> bTrans
         eData = bData <@ eDraw
     reactimate $ uncurry drawFunc <$> eData

-- | Creates an Observable of a buffer object from a subject of vertexes.
bufferObservable :: (U.BufferSource a) =>
                    Subject a ->
                    GL.BufferTarget ->
                    MomentIO (Observable GL.BufferObject)
bufferObservable s t = mapObservableIO (asObservable s) (U.fromSource t)
