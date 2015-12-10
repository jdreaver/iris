module Iris.DrawGraph
       ( DrawGraph (..)
       , GroupData (..)
       , defaultGroupData
       , drawGraph
       ) where


import Iris.Transformation

-- | A draw graph is a tree where transformations are passed down to child
-- nodes. Only leaf nodes can actually be drawn. Intermediate nodes can hold
-- zero or more children, and can optionally modify the transformation, but
-- should not directly draw primitives (although this is not enforced).
data DrawGraph = GroupNode GroupData [DrawGraph]
               | DrawableNode DrawFunc


data GroupData = GroupData
  { preDrawFunc :: IO ()  -- Maybe just have drawFunc and postDrawFunc
  , postDrawFunc :: IO ()
  , transformation :: Maybe Transformation
  }

type DrawFunc = Transformation -> IO ()


defaultGroupData :: GroupData
defaultGroupData = GroupData (return ()) (return ()) Nothing

drawGraph :: DrawGraph -> IO ()
drawGraph = drawGraph' identity

drawGraph' :: Transformation -> DrawGraph -> IO ()
drawGraph' t (DrawableNode f) = f t
drawGraph' t (GroupNode (GroupData preF postF mt) gs) = preF >> drawChildren >> postF
  where t' = maybe t (t `apply`) mt
        drawChildren = mapM_ (drawGraph' t') gs
