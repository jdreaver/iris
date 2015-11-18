-- | Defines a triangle item for plots

module Iris.Visuals.Triangle
       ( TriangleItem (..)
       , TriangleVertices
       , TriangleSpec (..)
       , triangleSpec
       , triangleInit
       ) where

import           Control.Lens
import qualified Graphics.GLUtil as U
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L
import           Reactive.Banana
import           Reactive.Banana.Frameworks

import Iris.Colors
import Iris.Reactive
import Iris.SceneGraph
import Iris.Transformation
import Iris.Visuals.Line (fsSource, vsSource)


-- | Shader program and buffer objects for a triangle
data TriangleItem = TriangleItem U.ShaderProgram GL.BufferObject TriangleVertices Color

type TriangleVertices = [L.V2 GL.GLfloat]

data TriangleSpec = TriangleSpec
  { triangleSpecVertices :: [L.V2 GL.GLfloat]
  , triangleSpecColors   :: Color
  }

triangleSpec :: TriangleSpec
triangleSpec = TriangleSpec [] (L.V3 1 1 1)

-- | Create triangle visual from a TriangleSpec
triangleInit :: TriangleSpec -> MomentIO Visual
triangleInit (TriangleSpec verts' c) =
  do prog <- liftIO $ U.simpleShaderProgramBS vsSource fsSource
     vs   <- subject verts'
     vbuf <- triangleBuff vs
     let bItem = TriangleItem <$> pure prog
                              <*> vbuf ^. behavior
                              <*> vs ^. behavior
                              <*> pure c
         f :: Event () -> Behavior Transformation -> MomentIO ()
         f eDraw bTrans = do let bData = (,) <$> bItem <*> bTrans
                                 eData = bData <@ eDraw
                             reactimate $ uncurry drawTriangle <$> eData
     return $ Visual f

triangleBuff :: Subject [L.V2 GL.GLfloat] ->          -- ^ Input vertices subject
                MomentIO (Observable GL.BufferObject) -- ^ Resulting buffer observable
triangleBuff s = mapObservableIO (asObservable s) (U.makeBuffer GL.ArrayBuffer)


-- | Draw a given triangle item to the current OpenGL context
drawTriangle :: TriangleItem -> L.M44 GL.GLfloat -> IO ()
drawTriangle (TriangleItem prog vbuf verts' color') m =
  do GL.currentProgram $= Just (U.program prog)
     U.enableAttrib prog "coord2d"

     GL.bindBuffer GL.ArrayBuffer $= Just vbuf
     U.setAttrib prog "coord2d"
        GL.ToFloat $ GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0
     U.asUniform m $ U.getUniform prog "mvp"
     U.asUniform color' $ U.getUniform prog "f_color"

     GL.drawArrays GL.Triangles 0 (fromIntegral $ length verts')

     GL.vertexAttribArray (U.getAttrib prog "coord2d") $= GL.Disabled
