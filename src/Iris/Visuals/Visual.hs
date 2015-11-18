-- | Base module for all visuals

module Iris.Visuals.Visual
       ( drawVisual
       ) where


import Reactive.Banana
import Reactive.Banana.Frameworks

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
