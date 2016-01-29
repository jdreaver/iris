-- | Wrappers around common OpenGL drawing functions.

module Iris.Draw
       ( bindVertexBuffer
       , bindElementBuffer
       , enableProgram
       , U.enableAttrib
       , disableAttrib
       , withAttrib
       , U.setUniform
       , Viewport (..)
       , insideViewport
       ) where

import qualified Graphics.GLUtil as U
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL


bindVertexBuffer :: U.ShaderProgram -> String -> GL.BufferObject -> GL.NumComponents -> IO ()
bindVertexBuffer prog var buffer ndim =
  do GL.bindBuffer GL.ArrayBuffer $= Just buffer
     U.setAttrib prog var
        GL.ToFloat $ GL.VertexArrayDescriptor ndim GL.Float 0 U.offset0


bindElementBuffer :: GL.BufferObject -> IO ()
bindElementBuffer buffer = GL.bindBuffer GL.ElementArrayBuffer $= Just buffer

enableProgram :: U.ShaderProgram -> IO ()
enableProgram prog = GL.currentProgram $= Just (U.program prog)

disableAttrib :: U.ShaderProgram -> String -> IO ()
disableAttrib prog name = GL.vertexAttribArray (U.getAttrib prog name) $= GL.Disabled

-- | Executes the IO action by first enabling an attribute, then performing the
-- action, then disabling the attribute.
withAttrib :: U.ShaderProgram -> String -> IO a -> IO a
withAttrib prog name f =
  do U.enableAttrib prog name
     result <- f
     disableAttrib prog name
     return result

-- | Holds the position of the top-left corner and the width/height of a
-- viewport.
data Viewport = Viewport
  { viewportPos  :: !GL.Position
  , viewportSize :: !GL.Size
  } deriving (Show)

-- | Determines if a given point is inside of a viewport.
insideViewport :: Viewport -> GL.GLint -> GL.GLint -> Bool
insideViewport (Viewport (GL.Position px py) (GL.Size pw ph)) x y =
  x >= px && x <= px + pw && y >= py && y <= py + ph
