{-# LANGUAGE OverloadedStrings #-}

-- | Visual to show an image overlaid on a rectangle

module Iris.Visuals.Image
       ( ImageItem (..)
       , U.readTexture
       , imageInit
       , imageFromFile
       ) where

import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V
import qualified Graphics.GLUtil as U
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L


import           Iris.SceneGraph

data ImageItem = ImageItem
  { imageProg         :: U.ShaderProgram
  , imageTex          :: GL.TextureObject
  , imageVertBuff     :: GL.BufferObject
  , imageFaceBuff     :: GL.BufferObject
  , imageTexCoordBuff :: GL.BufferObject
  , imageVerts        :: ImageVerts
  , imageFaces        :: ImageFaces
  }

type ImageVerts = V.Vector (L.V3 GL.GLfloat)
type ImageFaces = V.Vector (L.V3 GL.GLint)

data ImageSpec = ImageSpec GL.TextureObject


imageInit :: ImageSpec -> IO DrawNode
imageInit spec =
  do item <- makeImage spec
     return $ DrawNode (drawImage item)

makeImage :: ImageSpec -> IO ImageItem
makeImage (ImageSpec to) =
  do prog <- U.simpleShaderProgramBS vsSource fsSource
     vbo <- U.fromSource GL.ArrayBuffer verts
     fbo <- U.fromSource GL.ElementArrayBuffer faces
     tbo <- U.fromSource GL.ArrayBuffer texCoords
     return $ ImageItem prog to vbo fbo tbo verts faces

imageFromFile :: FilePath -> IO (Either String ImageSpec)
imageFromFile filePath =
  do to <- U.readTexture filePath
     case to of
       (Left err)  -> return (Left err)
       (Right to') -> do GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
                         U.texture2DWrap $= (GL.Mirrored, GL.ClampToEdge)
                         return $ Right (ImageSpec to')

drawImage :: ImageItem -> DrawFunc
drawImage (ImageItem prog to vbo fbo tbo _ fs) (DrawData t _) =
  do GL.currentProgram $= Just (U.program prog)

     U.enableAttrib prog "coord3d"
     GL.bindBuffer GL.ArrayBuffer $= Just vbo
     U.setAttrib prog "coord3d"
        GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0
     GL.bindBuffer GL.ElementArrayBuffer $= Just fbo

     U.enableAttrib prog "texcoord"
     GL.bindBuffer GL.ArrayBuffer $= Just tbo
     U.setAttrib prog "texcoord"
        GL.ToFloat $ GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0

     U.asUniform t $ U.getUniform prog "mvp"

     U.withTextures2D [to] $
       do U.asUniform (0 :: GL.GLint) $ U.getUniform prog "mytexture"
          U.drawIndexedTris (fromIntegral $ V.length fs)

     GL.vertexAttribArray (U.getAttrib prog "coord3d") $= GL.Disabled
     GL.vertexAttribArray (U.getAttrib prog "texcoord") $= GL.Disabled


vsSource, fsSource :: BS.ByteString
vsSource = BS.intercalate "\n"
           [ "attribute vec3 coord3d;"
           , "attribute vec2 texcoord;"
           , "varying vec2 f_texcoord;"
           , "uniform mat4 mvp;"
           , ""
           , "void main(void) {"
           , "    gl_Position = mvp * vec4(coord3d, 1.0);"
           , "    f_texcoord = texcoord;"
           , "}"
           ]

fsSource = BS.intercalate "\n"
           [ "varying vec2 f_texcoord;"
           , "uniform sampler2D mytexture;"
           , ""
           , "void main(void) {"
           , "    gl_FragColor = texture2D(mytexture, f_texcoord);"
           --, "    gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);"
           , "}"
           ]


verts :: ImageVerts
verts = V.fromList [ L.V3 (-0.5) 0.5 0
                   , L.V3 0.5    0.5 0
                   , L.V3 0.5  (-0.5) 0
                   , L.V3 (-0.5) (-0.5) 0
                   ]

faces :: ImageFaces
faces = V.fromList [ L.V3 0 1 2
                   , L.V3 2 3 0
                   ]


texCoords :: V.Vector (L.V2 GL.GLfloat)
texCoords = V.fromList [ L.V2 0 0
                       , L.V2 1 0
                       , L.V2 1 1
                       , L.V2 0 1
                       ]
