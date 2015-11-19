-- | Defines a system to handle events.

module Iris.Events
       ( EventHandler
       , EventHandled (..)
       , eventHandler
       , handleEvent
       ) where


import           Iris.Reactive

type EventHandler a = Event a -> MomentIO EventHandled


-- | If an event is accepted, subsequent event handlers will not be able to use
-- the event.
data EventHandled = Accepted | NotAccepted

-- | Iterate through event handlers and handle the given event.
handleEvent :: [EventHandler a] -> Event a -> MomentIO ()
handleEvent hs e = handleEvent' hs e NotAccepted

handleEvent' :: [EventHandler a] -> Event a -> EventHandled -> MomentIO ()
handleEvent' _ _ Accepted = return ()  -- Previous handler accepted
handleEvent' [] _ _       = return ()
handleEvent' (h:hs) e _   = h e >>= handleEvent' hs e

-- | Creates an event handler and returns the event handler with an event that
-- will be fired when the event handler is called.
eventHandler :: EventHandled -> MomentIO (Event a, EventHandler a)
eventHandler handled =
  do (e, f) <- newEvent
     let h e' = reactimate (f <$> e') >> return handled
     return (e, h)
