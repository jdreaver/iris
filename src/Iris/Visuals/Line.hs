{-# LANGUAGE OverloadedStrings #-}

-- | Defines data and functions for drawing a line.

module Iris.Visuals.Line
       ( LineItem
       , LineVertices
       , vsSource
       , fsSource

       , LineSpec (..)
       , lineSpec
       , lineInit
       ) where

import           Control.Lens
import qualified Data.ByteString as BS
import qualified Graphics.GLUtil as U
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L
import           Reactive.Banana
import           Reactive.Banana.Frameworks

import Iris.Colors
import Iris.Reactive
import Iris.Transformation
import Iris.SceneGraph


-- | Shader program and buffer objects for a line
data LineItem = LineItem U.ShaderProgram GL.BufferObject LineVertices Color

-- | Input vertices for a line buffer object
type LineVertices = [L.V2 GL.GLfloat]

data LineSpec = LineSpec
  { lineSpecVertices :: [L.V2 GL.GLfloat]
  , lineSpecColors   :: Color
  }

lineSpec :: LineSpec
lineSpec = LineSpec [] (L.V3 1 1 1)

-- | Create line visual from a LineSpec
lineInit :: LineSpec -> MomentIO Visual
lineInit (LineSpec verts' color') =
  do prog <- liftIO $ U.simpleShaderProgramBS vsSource fsSource
     vs   <- subject verts'
     vbuf <- lineBuff vs
     let bItem = LineItem <$> pure prog
                          <*> vbuf ^. behavior
                          <*> vs ^. behavior
                          <*> pure color'
         f :: Event () -> Behavior Transformation -> MomentIO ()
         f eDraw bTrans = do let bData = (,) <$> bItem <*> bTrans
                                 eData = bData <@ eDraw
                             reactimate $ uncurry drawLine <$> eData
     return $ Visual f

lineBuff :: Subject [L.V2 GL.GLfloat] ->          -- ^ Input vertices subject
            MomentIO (Observable GL.BufferObject) -- ^ Resulting buffer observable
lineBuff s = mapObservableIO (asObservable s) (U.makeBuffer GL.ArrayBuffer)

-- | Draw a given line item to the current OpenGL context
drawLine :: LineItem -> L.M44 GL.GLfloat -> IO ()
drawLine (LineItem prog vbuf verts' color') m =
  do GL.currentProgram $= Just (U.program prog)
     U.enableAttrib prog "coord2d"

     GL.bindBuffer GL.ArrayBuffer $= Just vbuf
     U.setAttrib prog "coord2d"
        GL.ToFloat $ GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0
     U.asUniform m $ U.getUniform prog "mvp"
     U.asUniform color' $ U.getUniform prog "f_color"

     GL.drawArrays GL.LineStrip 0 (fromIntegral $ length verts')
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
