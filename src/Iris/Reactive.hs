{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Utility module for functional reactive programming.

module Iris.Reactive
       ( Observable (..)
       , Subject (..)
       , behavior
       , event
       , handler
       , tVarSubject
       ) where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad (void)
import           Reactive.Banana
import           Reactive.Banana.Frameworks


-- | Wrapper around Event/Behavior pairs. Usually created in lieu of just an
-- Event when there is a meaningful value over time that is associated with the
-- event, like the cursor position or the window size.
data Observable a = Observable
  { _observableBehavior :: Behavior a
  , _observableEvent    :: Event a
  }
makeFields ''Observable


-- | Wrapper around a triple of Behavior, Event, and Handler. This is useful
-- over an `Observable` when the value of the Behavior is meant to be set from
-- the outside.
data Subject a = Subject
  { _subjectBehavior :: Behavior a
  , _subjectEvent    :: Event a
  , _subjectHandler  :: Handler a
  }
makeFields ''Subject


-- | Wrap a TVar with a reactive-banana Behavior/Event/Handler. Note that this
-- works only when the TVar is changed with the given Handler.
tVarSubject :: TVar a -> MomentIO (Subject a)
tVarSubject tvar =
  do (e, h) <- newEvent
     currentVal <- liftIO $ readTVarIO tvar
     b <- stepper currentVal e
     reactimate $ (void . atomically . writeTVar tvar) <$> e
     return $ Subject b e h
