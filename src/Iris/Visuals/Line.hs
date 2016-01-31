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
       , makeLine
       ) where

import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L

import           Iris.OpenGL (ShaderProgram, simpleShaderProgramBS,
                              enableProgram, enableAttrib, setUniform,
                              disableAttrib, bindVertexBuffer, fromVector)
import           Iris.SceneGraph


-- | Shader program and buffer objects for a line
data LineItem = LineItem ShaderProgram GL.BufferObject LineVertices LineColor

-- | Input vertices for a line buffer object
type LineVertices = V.Vector (L.V3 GL.GLfloat)

type LineColor = L.V3 GL.GLfloat

data LineSpec = LineSpec
  { lineSpecVertices :: LineVertices
  , lineSpecColors   :: LineColor
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
  do prog <- simpleShaderProgramBS vsSource fsSource
     vbuf <- fromVector GL.ArrayBuffer verts'
     return $ LineItem prog vbuf verts' color'


-- | Draw a given line item to the current OpenGL context
drawLine :: LineItem -> DrawFunc
drawLine (LineItem prog vbuf verts' color') (DrawData t _) =
  do enableProgram prog

     enableAttrib prog "coord3d"
     bindVertexBuffer prog "coord3d" vbuf 3

     setUniform prog "mvp" t
     setUniform prog "f_color" color'

     GL.drawArrays GL.LineStrip 0 (fromIntegral $ V.length verts')

     disableAttrib prog "coord3d"


vsSource, fsSource :: BS.ByteString
vsSource = BS.intercalate "\n"
           [
             "attribute vec3 coord3d;"
           , "uniform mat4 mvp;"
           , ""
           , "void main(void) { "
           , "    gl_Position = mvp * vec4(coord3d, 1.0); "
           , "}"
           ]

fsSource = BS.intercalate "\n"
           [
             "uniform vec3 f_color;"
           , "void main(void) { "
           , "    gl_FragColor = vec4(f_color.xyz, 1.0);"
           , "}"
           ]
