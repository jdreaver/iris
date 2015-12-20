-- | Pure tree structure for drawing.

module Iris.SceneGraph.DrawGraph
       ( DrawNode (..)
       , DrawFunc
       , drawGraph
       , groupNode
       , transNode
       , effectNode
       ) where


import Iris.Transformation

type DrawFunc = Transformation -> IO ()

-- | A DrawNode is the basis for creating a tree of drawable items. The
-- fundamental piece of information that is transmitted across a draw graph is
-- the current transformation. This data type is very barebones; see the other
-- functions in this module to create "useful" draw nodes.
data DrawNode = DrawNode
  { drawFunc :: DrawFunc
  }

-- | Draws a tree of DrawNodes.
drawGraph :: DrawNode -> IO ()
drawGraph = drawGraph' identity

drawGraph' :: Transformation -> DrawNode -> IO ()
drawGraph' t (DrawNode f) = f t

-- | Creates a node that simply holds a set of children.
groupNode :: [DrawNode] -> DrawNode
groupNode cs = DrawNode (drawChildren cs)

drawChildren :: [DrawNode] -> DrawFunc
drawChildren cs trans = mapM_ (drawGraph' trans) cs

-- | Modifies the current transformation, and then draws children.
transNode :: Transformation -> [DrawNode] -> DrawNode
transNode t cs = DrawNode (drawTrans t cs)

drawTrans :: Transformation -> [DrawNode] -> DrawFunc
drawTrans t' cs t = drawChildren cs (t `apply` t')

-- | Performs some IO effect before drawing children. Useful for OpenGL setup
-- functions.
effectNode :: IO () -> [DrawNode] -> DrawNode
effectNode f cs = DrawNode (\t -> f >> drawChildren cs t)
