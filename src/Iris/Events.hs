-- | Defines a system to handle events.

module Iris.Events
       ( EventHandler
       , fromHandler
       ) where


import           Iris.Reactive

-- | An EventHandler is simply a function that takes an input event, and does
-- something with it in the MomentIO monad.
type EventHandler a = Event a -> MomentIO ()

-- | Creates an EventHandler from a Handler. Basically just hooks up the input
-- event to fire the handler using reactimate.
fromHandler :: Handler a -> EventHandler a
fromHandler h e = reactimate $ fmap h e
