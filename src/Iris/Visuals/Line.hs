{-# LANGUAGE OverloadedStrings #-}

-- | Defines data and functions for drawing a line.

module Iris.Visuals.Line
       ( LineItem (..)
       , LineVertices
       , vsSource
       , fsSource
       , drawLine

       , LineSpec (..)
       , lineSpec
       , lineInit
       ) where

import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V
import qualified Graphics.GLUtil as U
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L

import Iris.Colors
import Iris.Draw
import Iris.SceneGraph


-- | Shader program and buffer objects for a line
data LineItem = LineItem U.ShaderProgram GL.BufferObject LineVertices Color

-- | Input vertices for a line buffer object
type LineVertices = V.Vector (L.V2 GL.GLfloat)

data LineSpec = LineSpec
  { lineSpecVertices :: LineVertices
  , lineSpecColors   :: Color
  }

lineSpec :: LineSpec
lineSpec = LineSpec V.empty (L.V3 1 1 1)

-- | Create line visual from a LineSpec
lineInit :: LineSpec -> IO DrawNode
lineInit spec =
  do item <- makeLine spec
     return $ DrawNode (drawLine item)

makeLine :: LineSpec -> IO LineItem
makeLine (LineSpec verts' color') =
  do prog <- U.simpleShaderProgramBS vsSource fsSource
     vbuf <- U.fromSource GL.ArrayBuffer verts'
     return $ LineItem prog vbuf verts' color'


-- | Draw a given line item to the current OpenGL context
drawLine :: LineItem -> DrawFunc
drawLine (LineItem prog vbuf verts' color') (DrawData t _) =
  do enableProgram prog

     enableAttrib prog "coord2d"
     bindVertexBuffer prog "coord2d" vbuf 2

     setUniform prog "mvp" t
     setUniform prog "f_color" color'

     GL.drawArrays GL.LineStrip 0 (fromIntegral $ V.length verts')

     disableAttrib prog "coord2d"


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
