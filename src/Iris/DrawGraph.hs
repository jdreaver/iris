module Iris.DrawGraph
       ( DrawGraph (..)
       , GroupData (..)
       , DrawFunc
       , defaultGroupData
       , drawGraph
       ) where


import Iris.Transformation

-- | A draw graph is a tree where transformations are passed down to child
-- nodes. Only leaf nodes can actually be drawn. Intermediate nodes can hold
-- zero or more children, and can optionally modify the transformation, but
-- should not directly draw primitives (although this is not enforced).
data DrawGraph = GroupNode GroupData [DrawGraph]
               | TransformNode Transformation DrawGraph
               | DrawableNode DrawFunc


data GroupData = GroupData
  { preDrawFunc :: IO ()  -- Maybe just have drawFunc and postDrawFunc. Or, add
                          -- these funcs as child visuals, so GroupData is not
                          -- even needed.
  , postDrawFunc :: IO ()
  }

type DrawFunc = Transformation -> IO ()


defaultGroupData :: GroupData
defaultGroupData = GroupData (return ()) (return ())

drawGraph :: DrawGraph -> IO ()
drawGraph = drawGraph' identity

drawGraph' :: Transformation -> DrawGraph -> IO ()
drawGraph' t (DrawableNode f) = f t
drawGraph' t (TransformNode t' g) = drawGraph' (t `apply` t') g
drawGraph' t (GroupNode (GroupData preF postF) gs) = preF >> drawChildren >> postF
  where drawChildren = mapM_ (drawGraph' t) gs
