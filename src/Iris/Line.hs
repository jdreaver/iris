{-# LANGUAGE OverloadedStrings #-}

-- | Defines data and functions for drawing a line.

module Iris.Line
       ( lineItem
       , LineItem
       , LineVertices
       , vsSource
       , fsSource
       ) where

import qualified Data.ByteString as BS
import qualified Graphics.GLUtil as U
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L

import Iris.Colors
import Iris.SceneGraph

-- | Shader program and buffer objects for a line
data LineItem = LineItem U.ShaderProgram GL.BufferObject LineVertices Color

-- | Input vertices for a line buffer object
type LineVertices = [L.V2 GL.GLfloat]

-- | Create a shader program for a line and a PlotItem that can be plotted.
lineItem :: LineVertices -> Color -> IO PlotItem
lineItem verts color =
  do prog <- initLine verts color
     return $ PlotItem (drawLine prog)

-- | Create a line program
initLine :: LineVertices -> Color -> IO LineItem
initLine vertices color =
  do prog <- U.simpleShaderProgramBS vsSource fsSource
     vbuf <- U.makeBuffer GL.ArrayBuffer vertices
     return $ LineItem prog vbuf vertices color

-- | Draw a given line program to the current OpenGL context
drawLine :: LineItem -> L.M44 GL.GLfloat -> IO ()
drawLine (LineItem prog vbuf verts color) m =
  do GL.currentProgram $= Just (U.program prog)
     U.enableAttrib prog "coord2d"

     GL.bindBuffer GL.ArrayBuffer $= Just vbuf
     U.setAttrib prog "coord2d"
        GL.ToFloat $ GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0
     U.asUniform m $ U.getUniform prog "mvp"
     U.asUniform color $ U.getUniform prog "f_color"

     GL.drawArrays GL.LineStrip 0 (fromIntegral $ length verts)
     GL.vertexAttribArray (U.getAttrib prog "coord2d") $= GL.Disabled


vsSource, fsSource :: BS.ByteString
vsSource = BS.intercalate "\n"
           [
             "attribute vec2 coord2d; "
           , "uniform mat4 mvp;"
           , ""
           , "void main(void) { "
           , "    gl_Position = mvp * vec4(coord2d, 0.0, 1.0); "
           , "}"
           ]

fsSource = BS.intercalate "\n"
           [
             "uniform vec3 f_color;"
           , "void main(void) { "
           , "    gl_FragColor = vec4(f_color.xyz, 1.0);"
           , "}"
           ]