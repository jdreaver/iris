-- | Defines a mesh item for plots

module Iris.Visuals.Mesh
       ( MeshItem (..)
       , MeshVertices
       , MeshSpec (..)
       , meshSpec
       , meshInit
       ) where

import           Control.Lens
import qualified Graphics.GLUtil as U
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L
import           Reactive.Banana.Frameworks

import Iris.Colors
import Iris.Reactive
import Iris.SceneGraph
import Iris.Visuals.Line (fsSource, vsSource)
import Iris.Visuals.Visual

-- | Shader program and buffer objects for a mesh
data MeshItem = MeshItem U.ShaderProgram GL.BufferObject MeshVertices Color

type MeshVertices = [L.V2 GL.GLfloat]

data MeshSpec = MeshSpec
  { meshSpecVertices :: [L.V2 GL.GLfloat]
  , meshSpecColors   :: Color
  }

meshSpec :: MeshSpec
meshSpec = MeshSpec [] (L.V3 1 1 1)

-- | Create mesh visual from a MeshSpec
meshInit :: MeshSpec -> MomentIO Visual
meshInit (MeshSpec verts' c) =
  do prog <- liftIO $ U.simpleShaderProgramBS vsSource fsSource
     vs   <- subject verts'
     vbuf <- bufferObservable vs
     let bItem = MeshItem <$> pure prog
                          <*> vbuf ^. behavior
                          <*> vs ^. behavior
                          <*> pure c
     return $ Visual (drawVisual bItem drawMesh)


-- | Draw a given mesh item to the current OpenGL context
drawMesh :: MeshItem -> L.M44 GL.GLfloat -> IO ()
drawMesh (MeshItem prog vbuf verts' color') m =
  do GL.currentProgram $= Just (U.program prog)
     U.enableAttrib prog "coord2d"

     GL.bindBuffer GL.ArrayBuffer $= Just vbuf
     U.setAttrib prog "coord2d"
        GL.ToFloat $ GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0
     U.asUniform m $ U.getUniform prog "mvp"
     U.asUniform color' $ U.getUniform prog "f_color"

     GL.drawArrays GL.Triangles 0 (fromIntegral $ length verts')

     GL.vertexAttribArray (U.getAttrib prog "coord2d") $= GL.Disabled
