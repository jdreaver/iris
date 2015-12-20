-- | Pure tree structure for drawing.

module Iris.SceneGraph.DrawGraph
       ( DrawNode (..)
       , DrawData (..)
       , DrawFunc
       , drawGraph
       , groupNode
       , transNode
       , effectNode
       ) where


import qualified Graphics.Rendering.OpenGL as GL

import           Iris.Transformation

-- | DrawData is passed between and possible modified by nodes in the scene
-- graph.
data DrawData = DrawData
  { transform :: Transformation
  , viewport :: GL.Size
  } deriving (Show)

drawData :: DrawData
drawData = DrawData identity (GL.Size 2 2)


-- | A DrawNode is the basis for creating a tree of drawable items. The
-- fundamental piece of information that is transmitted across a draw graph is
-- the current transformation. This data type is very barebones; see the other
-- functions in this module to create "useful" draw nodes.
data DrawNode = DrawNode
  { drawFunc :: DrawFunc
  }

type DrawFunc = DrawData -> IO ()

-- | Draws a tree of DrawNodes.
drawGraph :: DrawNode -> IO ()
drawGraph = drawGraph' drawData

drawGraph' :: DrawData -> DrawNode -> IO ()
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
drawTrans t' cs (DrawData t s) = drawChildren cs (DrawData (t `apply` t') s)

-- | Performs some IO effect before drawing children. Useful for OpenGL setup
-- functions.
effectNode :: IO () -> [DrawNode] -> DrawNode
effectNode f cs = DrawNode (\t -> f >> drawChildren cs t)
