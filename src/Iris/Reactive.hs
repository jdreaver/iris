-- | Utility module for reactive programming

module Iris.Reactive
       ( tVarBehavior
       ) where

import           Control.Concurrent.STM
import           Control.Monad (void)
import           Reactive.Banana
import           Reactive.Banana.Frameworks


-- | Wrap a TVar with a reactive-banana Behavior/Event/Handler. Note that this
-- works only when the TVar is changed with the given Handler.
tVarBehavior :: TVar a -> MomentIO (Behavior a, Event a, Handler a)
tVarBehavior tvar =
  do (event, handler) <- newEvent
     currentVal <- liftIO $ readTVarIO tvar
     behavior <- stepper currentVal event
     reactimate $ (void . atomically . writeTVar tvar) <$> event
     return (behavior, event, handler)
