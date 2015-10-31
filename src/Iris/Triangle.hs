-- | Defines a triangle item for plots

module Iris.Triangle
       ( TriangleProgram (..)
       , TriangleVertices
       , initTriangle
       , drawTriangle
       ) where

import qualified Data.ByteString as BS
import qualified Graphics.GLUtil as U
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L

import Iris.Line (fsSource, vsSource)

type TriangleVertices = [L.V2 GL.GLfloat]

-- | Shader program and buffer objects for a triangle
data TriangleProgram = TriangleProgram U.ShaderProgram GL.BufferObject TriangleVertices

-- | Create a triangle program
initTriangle :: TriangleVertices -> IO TriangleProgram
initTriangle vertices =
  do prog <- U.simpleShaderProgramBS vsSource fsSource
     vbuf <- U.makeBuffer GL.ArrayBuffer vertices
     return $ TriangleProgram prog vbuf vertices


-- | Draw a given triangle program to the current OpenGL context
drawTriangle :: TriangleProgram -> L.M44 GL.GLfloat -> IO ()
drawTriangle (TriangleProgram prog vbuf verts) m =
  do GL.currentProgram $= Just (U.program prog)
     U.enableAttrib prog "coord2d"

     GL.bindBuffer GL.ArrayBuffer $= Just vbuf
     U.setAttrib prog "coord2d"
        GL.ToFloat $ GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0
     U.asUniform m $ U.getUniform prog "mvp"

     GL.drawArrays GL.Triangles 0 (fromIntegral $ length verts)

     GL.vertexAttribArray (U.getAttrib prog "coord2d") $= GL.Disabled
