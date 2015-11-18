-- | Defines a mesh item for plots

module Iris.Visuals.Mesh
       ( MeshItem (..)
       , MeshVertices
       , MeshFaceIndices
       , MeshFaceVertices
       , MeshSpec (..)
       , MeshData (..)
       , meshSpec
       , meshInit
       ) where

import           Control.Lens
import qualified Data.Vector.Storable as V
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
data MeshItem = MeshItem U.ShaderProgram MeshDataBuffer Color

data MeshSpec = MeshSpec
  { meshSpecData   :: MeshData
  , meshSpecColors :: Color
  }

meshSpec :: MeshSpec
meshSpec = MeshSpec (Vertexes $ V.fromList []) (L.V3 1 1 1)

type MeshVertices = V.Vector (L.M33 GL.GLfloat)
type MeshFaceVertices = V.Vector (L.V3 GL.GLfloat)
type MeshFaceIndices = V.Vector (L.V3 GL.GLint)

data MeshData = Vertexes MeshVertices
              | Faces MeshFaceVertices MeshFaceIndices

data MeshDataBuffer = VertexesBuffer MeshVertices GL.BufferObject
                    | FacesBuffer MeshFaceVertices MeshFaceIndices
                      GL.BufferObject GL.BufferObject

-- | Create mesh visual from a MeshSpec
meshInit :: MeshSpec -> MomentIO Visual
meshInit (MeshSpec md c) =
  do prog <- liftIO $ U.simpleShaderProgramBS vsSource fsSource
     vbuf <- meshBufferObservable md
     let bItem = MeshItem <$> pure prog
                          <*> vbuf ^. behavior
                          <*> pure c
     return $ Visual (drawVisual bItem drawMesh)


meshBufferObservable :: MeshData -> MomentIO (Observable MeshDataBuffer)
meshBufferObservable (Vertexes verts) =
  do vs  <- subject verts
     obs <- bufferObservable vs GL.ArrayBuffer
     return $ VertexesBuffer verts <$> obs
meshBufferObservable (Faces verts faces) =
  do vs <- subject verts
     fs <- subject faces
     vb <- bufferObservable vs GL.ArrayBuffer
     fb <- bufferObservable fs GL.ElementArrayBuffer
     return $ merge (FacesBuffer verts faces) vb fb

-- | Draw a given mesh item to the current OpenGL context
drawMesh :: MeshItem -> L.M44 GL.GLfloat -> IO ()
drawMesh (MeshItem prog meshData color') m =
  do GL.currentProgram $= Just (U.program prog)
     U.enableAttrib prog "coord2d"

     bindMeshData prog meshData

     U.asUniform m $ U.getUniform prog "mvp"
     U.asUniform color' $ U.getUniform prog "f_color"

     drawMeshData meshData

     GL.vertexAttribArray (U.getAttrib prog "coord2d") $= GL.Disabled


bindMeshData :: U.ShaderProgram -> MeshDataBuffer -> IO ()
bindMeshData p (VertexesBuffer _ vbo) =
  do GL.bindBuffer GL.ArrayBuffer $= Just vbo
     U.setAttrib p "coord2d"
        GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0
bindMeshData p (FacesBuffer _ _ vb fb) =
  do GL.bindBuffer GL.ArrayBuffer $= Just vb
     U.setAttrib p "coord2d"
        GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0
     GL.bindBuffer GL.ElementArrayBuffer $= Just fb

drawMeshData :: MeshDataBuffer -> IO ()
drawMeshData (VertexesBuffer v _) =
  GL.drawArrays GL.Triangles 0 (fromIntegral $ V.length v * 3)
drawMeshData (FacesBuffer _ fs _ _) =
  U.drawIndexedTris (fromIntegral $ V.length fs)
