{-# LANGUAGE OverloadedStrings #-}

-- | Defines a mesh item for plots

module Iris.Visuals.Mesh
       ( MeshItem (..)
       , MeshVertices
       , MeshFaceIndices
       , MeshFaceVertices
       , MeshSpec (..)
       , MeshColor (..)
       , MeshVectorColor
       , MeshData (..)
       , meshSpec
       , meshInit
       ) where

import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V
import qualified Graphics.GLUtil as U
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L

import Iris.Colors
import Iris.SceneGraph

-- | Shader program and buffer objects for a mesh
data MeshItem = MeshItem U.ShaderProgram MeshDataBuffer MeshColorBuffer

data MeshSpec = MeshSpec
  { meshSpecData   :: MeshData
  , meshSpecColors :: MeshColor
  } deriving (Show)

type MeshVectorColor = (V.Vector (L.V3 GL.GLfloat))
data MeshColor = ConstantMeshColor Color
               | VectorMeshColor MeshVectorColor
               deriving (Show)

data MeshColorBuffer = ConstantColorBuffer Color
                     | VectorColorBuffer MeshVectorColor GL.BufferObject

meshSpec :: MeshSpec
meshSpec = MeshSpec (Vertexes $ V.fromList []) (ConstantMeshColor $ L.V3 1 1 1)

type MeshVertices = V.Vector (L.M33 GL.GLfloat)
type MeshFaceVertices = V.Vector (L.V3 GL.GLfloat)
type MeshFaceIndices = V.Vector (L.V3 GL.GLint)

data MeshData = Vertexes MeshVertices
              | Faces MeshFaceVertices MeshFaceIndices
              deriving (Show)

data MeshDataBuffer = VertexesBuffer MeshVertices GL.BufferObject
                    | FacesBuffer MeshFaceVertices MeshFaceIndices
                      GL.BufferObject GL.BufferObject

-- | Create mesh visual from a MeshSpec
meshInit :: MeshSpec -> IO Visual
meshInit spec =
  do item <- makeMesh spec
     return $ Visual (drawMesh item)

makeMesh :: MeshSpec -> IO MeshItem
makeMesh (MeshSpec md c) =
  do prog <- meshProgram c
     vbuf <- meshBuffer md
     cbuf <- meshColorBuffer c
     return $ MeshItem prog vbuf cbuf

meshProgram :: MeshColor -> IO U.ShaderProgram
meshProgram c = U.simpleShaderProgramBS (vsSource c) (fsSource c)


meshBuffer :: MeshData -> IO MeshDataBuffer
meshBuffer (Vertexes verts) =
  do b <- U.fromSource GL.ArrayBuffer verts
     return $ VertexesBuffer verts b
meshBuffer (Faces verts faces) =
  do vb <- U.fromSource GL.ArrayBuffer verts
     fb <- U.fromSource GL.ElementArrayBuffer faces
     return $ FacesBuffer verts faces vb fb

meshColorBuffer :: MeshColor -> IO MeshColorBuffer
meshColorBuffer (ConstantMeshColor c) = return $ ConstantColorBuffer c
meshColorBuffer (VectorMeshColor cv) =
  do cb <- U.fromSource GL.ArrayBuffer cv
     return $ VectorColorBuffer cv cb

-- | Draw a given mesh item to the current OpenGL context
drawMesh :: MeshItem -> L.M44 GL.GLfloat -> IO ()
drawMesh (MeshItem prog meshData color') m =
  do GL.currentProgram $= Just (U.program prog)

     U.enableAttrib prog "coord3d"
     bindMeshData prog meshData

     U.asUniform m $ U.getUniform prog "mvp"
     drawMeshColor prog color'

     drawMeshData meshData

     GL.vertexAttribArray (U.getAttrib prog "coord3d") $= GL.Disabled

     disableColor prog color'


bindMeshData :: U.ShaderProgram -> MeshDataBuffer -> IO ()
bindMeshData p (VertexesBuffer _ vbo) =
  do GL.bindBuffer GL.ArrayBuffer $= Just vbo
     U.setAttrib p "coord3d"
        GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0
bindMeshData p (FacesBuffer _ _ vb fb) =
  do GL.bindBuffer GL.ArrayBuffer $= Just vb
     U.setAttrib p "coord3d"
        GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0
     GL.bindBuffer GL.ElementArrayBuffer $= Just fb

drawMeshData :: MeshDataBuffer -> IO ()
drawMeshData (VertexesBuffer v _) =
  GL.drawArrays GL.Triangles 0 (fromIntegral $ V.length v * 3)
drawMeshData (FacesBuffer _ fs _ _) =
  U.drawIndexedTris (fromIntegral $ V.length fs)


drawMeshColor :: U.ShaderProgram -> MeshColorBuffer -> IO ()
drawMeshColor prog (ConstantColorBuffer c) =
  U.asUniform c $ U.getUniform prog "f_color"
drawMeshColor prog (VectorColorBuffer _ cb) =
  do U.enableAttrib prog "v_color"
     GL.bindBuffer GL.ArrayBuffer $= Just cb
     U.setAttrib prog "v_color"
        GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0

disableColor :: U.ShaderProgram -> MeshColorBuffer -> IO ()
disableColor _ (ConstantColorBuffer _) = return ()
disableColor prog (VectorColorBuffer _ _) =
  GL.vertexAttribArray (U.getAttrib prog "v_color") $= GL.Disabled

vsSource, fsSource :: MeshColor -> BS.ByteString
vsSource mc = BS.intercalate "\n"
              [
                "attribute vec3 coord3d; "
              , colorVertexLine mc
              , "uniform mat4 mvp;"
              , "void main(void) { "
              , "    gl_Position = mvp * vec4(coord3d, 1.0); "
              , colorVertexFunc mc
              , "}"
              ]

fsSource mc = BS.intercalate "\n"
              [
                colorFragmentLine mc
              , "void main(void) { "
              , "    gl_FragColor = vec4(f_color.xyz, 1.0);"
              , "}"
              ]


colorVertexLine :: MeshColor -> BS.ByteString
colorVertexLine (ConstantMeshColor _) = ""
colorVertexLine (VectorMeshColor _) = "attribute vec3 v_color;\nvarying vec3 f_color;"

colorVertexFunc :: MeshColor -> BS.ByteString
colorVertexFunc (ConstantMeshColor _) = ""
colorVertexFunc (VectorMeshColor _) = "    f_color = v_color;"

colorFragmentLine :: MeshColor -> BS.ByteString
colorFragmentLine (ConstantMeshColor _) = "uniform vec3 f_color;"
colorFragmentLine (VectorMeshColor _) = "varying vec3 f_color;"
