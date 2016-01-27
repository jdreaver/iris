{-# LANGUAGE DeriveFunctor #-}

-- | Utility module for functional reactive programming.

module Iris.Reactive
       ( Observable (..)
       , Subject (..)
       , asObservable
       , mapObservableIO
       , merge
       , subject
       , subjectIO
       , tVarSubject
       , mapSuccE
       , module Reactive.Banana
       , module Reactive.Banana.Frameworks
       ) where

import           Control.Concurrent.STM
import           Control.Monad (void, (>=>))
import           Reactive.Banana
import           Reactive.Banana.Frameworks


-- | Wrapper around Event/Behavior pairs. Usually created in lieu of just an
-- Event when there is a meaningful value over time that is associated with the
-- event, like the cursor position or the window size.
data Observable a = Observable
  { observableBehavior :: Behavior a
  , observableEvent    :: Event a
  } deriving (Functor)

-- | Combines two Observables
merge :: (a -> b -> c) -> Observable a -> Observable b -> Observable c
merge f (Observable b1 e1) (Observable b2 e2) =
  let b   = f <$> b1 <*> b2
      e12 = (f      <$> b1) `apply` e2
      e21 = (flip f <$> b2) `apply` e1
      e   = unionWith const e12 e21
  in Observable b e

-- | Wrapper around a triple of Behavior, Event, and Handler. This is useful
-- over an `Observable` when the value of the Behavior is meant to be set from
-- the outside.
data Subject a = Subject
  { subjectBehavior :: Behavior a
  , subjectEvent    :: Event a
  , subjectHandler  :: Handler a
  }

-- | Create subject from initial value
subject :: a -> MomentIO (Subject a)
subject x0 =
  do (e, h) <- newEvent
     b <- stepper x0 e
     return $ Subject b e h

-- | Create a subject from an initial value, except create the handler in IO
subjectIO :: a -> IO (Handler a, MomentIO (Subject a))
subjectIO x0 =
  do (ah, h) <- newAddHandler
     let s =  do e <- fromAddHandler ah
                 b <- stepper x0 e
                 return $ Subject b e h
     return (h, s)

-- | View a subject as an observable. This makes the behavior/event pair
-- "read-only", as it hides the Handler to trigger the event.
asObservable :: Subject a -> Observable a
asObservable (Subject b e _) = Observable b e

-- | Create a new Observable by mapping an IO function over the old observable.
mapObservableIO :: Observable a -> (a -> IO b) -> MomentIO (Observable b)
mapObservableIO (Observable b e) f =
  do v0 <- valueB b
     x0 <- liftIO $ f v0
     s  <- subject x0
     reactimate $ (f >=> subjectHandler s) <$> e
     return (asObservable s)

-- | Wrap a TVar with a reactive-banana Behavior/Event/Handler. Note that this
-- works only when the TVar is changed with the given Handler.
tVarSubject :: TVar a -> MomentIO (Subject a)
tVarSubject tvar =
  do currentVal <- liftIO $ readTVarIO tvar
     s <- subject currentVal
     reactimate $ (void . atomically . writeTVar tvar) <$> subjectEvent s
     return s

-- | Apply a function to successive values of an event stream to produce a new
-- event.
mapSuccE :: (a -> a -> b) -> a -> Event a -> Moment (Event b)
mapSuccE f x0 event =
  do pairs <- accumE (x0, x0) $ (\e' (_, e) -> (e, e')) <$> event
     return $ uncurry f <$> pairs
