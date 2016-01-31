{-# LANGUAGE ScopedTypeVariables #-}

-- | Wrapper around OpenGL buffer objects. A lot of code taken from GLUtil.

module Iris.OpenGL.BufferObjects
       ( bindVertexBuffer
       , bindElementBuffer
       , fromVector
       ) where


import qualified Data.Vector.Storable as V
import           Foreign.Ptr (wordPtrToPtr, Ptr)
import           Foreign.Storable (Storable, sizeOf)
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


-- | Fill a buffer with data from a 'V.Vector'.
fromVector :: forall a. Storable a
           => BufferTarget
           -> V.Vector a
           -> IO BufferObject
fromVector target v = V.unsafeWith v $ fromPtr target numBytes
  where numBytes = fromIntegral $ V.length v * sizeOf (undefined :: a)


-- | Allocate and fill a 'BufferObject' with the given number of bytes
-- from the supplied pointer.
fromPtr :: BufferTarget -> Int -> Ptr a -> IO BufferObject
fromPtr target numBytes ptr =
  do [buffer] <- genObjectNames 1
     bindBuffer target $= Just buffer
     bufferData target $= (fromIntegral numBytes, ptr, StaticDraw)
     return buffer
