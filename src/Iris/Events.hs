-- | Defines a system to handle events.

module Iris.Events
       ( EventHandler
       , EventHandled (..)
       , handleEvent
       ) where


import           Reactive.Banana
import           Reactive.Banana.Frameworks


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
