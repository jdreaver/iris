{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
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

#if !MIN_VERSION_base(4,8,0)
import           Prelude.Compat ((<$>))
#endif

import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V
import qualified Graphics.GLUtil as U
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L

import           Iris.Colors
import           Iris.OpenGL (ShaderProgram, simpleShaderProgramBS,
                              enableProgram, enableAttrib, setUniform,
                              disableAttrib, bindVertexBuffer,
                              bindElementBuffer)
import           Iris.SceneGraph

-- | Shader program and buffer objects for a mesh
data MeshItem = MeshItem ShaderProgram MeshDataBuffer MeshColorBuffer

data MeshSpec = MeshSpec
  { meshSpecData   :: !MeshData
  , meshSpecColors :: !MeshColor
  } deriving (Show)

type MeshVectorColor = (V.Vector (L.V3 GL.GLfloat))
data MeshColor = ConstantMeshColor Color
               | VectorMeshColor MeshVectorColor
               deriving (Show)

data MeshColorBuffer = ConstantColorBuffer Color
                     | VectorColorBuffer MeshVectorColor GL.BufferObject

meshSpec :: MeshSpec
meshSpec = MeshSpec (Vertexes []) (ConstantMeshColor $ L.V3 1 1 1)

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
meshInit :: MeshSpec -> IO DrawNode
meshInit spec =
  do item <- makeMesh spec
     return $ DrawNode (drawMesh item)

makeMesh :: MeshSpec -> IO MeshItem
makeMesh (MeshSpec md c) =
  do prog <- simpleShaderProgramBS (vsSource c) (fsSource c)
     vbuf <- meshBuffer md
     cbuf <- meshColorBuffer c
     return $ MeshItem prog vbuf cbuf


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
meshColorBuffer (VectorMeshColor cv) = VectorColorBuffer cv <$>
                                       U.fromSource GL.ArrayBuffer cv

-- | Draw a given mesh item to the current OpenGL context
drawMesh :: MeshItem -> DrawFunc
drawMesh (MeshItem prog meshData color') (DrawData t _) =
  do enableProgram prog

     enableAttrib prog "coord3d"
     bindMeshData prog meshData

     setUniform prog "mvp" t
     drawMeshColor prog color'

     drawMeshData meshData

     disableAttrib prog "coord3d"
     disableColor prog color'


bindMeshData :: ShaderProgram -> MeshDataBuffer -> IO ()
bindMeshData p (VertexesBuffer _ vbo) = bindVertexBuffer p "coord3d" vbo 3
bindMeshData p (FacesBuffer _ _ vb fb) = bindVertexBuffer p "coord3d" vb 3 >>
                                         bindElementBuffer fb

drawMeshData :: MeshDataBuffer -> IO ()
drawMeshData (VertexesBuffer v _) =
  GL.drawArrays GL.Triangles 0 (fromIntegral $ V.length v * 3)
drawMeshData (FacesBuffer _ fs _ _) =
  U.drawIndexedTris (fromIntegral $ V.length fs)


drawMeshColor :: ShaderProgram -> MeshColorBuffer -> IO ()
drawMeshColor p (ConstantColorBuffer c) = setUniform p "f_color" c
drawMeshColor p (VectorColorBuffer _ cb) = enableAttrib p "v_color" >>
                                           bindVertexBuffer p "v_color" cb 3

disableColor :: ShaderProgram -> MeshColorBuffer -> IO ()
disableColor _ (ConstantColorBuffer _) = return ()
disableColor p (VectorColorBuffer _ _) = disableAttrib p "v_color"

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
