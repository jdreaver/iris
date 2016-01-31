-- | Wrapper around OpenGL buffer objects.

module Iris.OpenGL.BufferObjects
       ( bindVertexBuffer
       , bindElementBuffer
       ) where


import           Foreign.Ptr (wordPtrToPtr, Ptr)
import           Graphics.Rendering.OpenGL

import           Iris.OpenGL.ShaderProgram (ShaderProgram, setAttrib)

-- | Produce a 'Ptr' value to be used as an offset of the given number of
-- bytes.
offsetPtr :: Int -> Ptr a
offsetPtr = wordPtrToPtr . fromIntegral

-- | A zero-offset 'Ptr'.
offset0 :: Ptr a
offset0 = offsetPtr 0

-- | Binds a vertex buffer to a named attribute.
bindVertexBuffer :: ShaderProgram -> String -> BufferObject -> NumComponents -> IO ()
bindVertexBuffer prog var buffer ndim =
  do bindBuffer ArrayBuffer $= Just buffer
     setAttrib prog var
        ToFloat $ VertexArrayDescriptor ndim Float 0 offset0

-- | Binds an element buffer.
bindElementBuffer :: BufferObject -> IO ()
bindElementBuffer buffer = bindBuffer ElementArrayBuffer $= Just buffer
