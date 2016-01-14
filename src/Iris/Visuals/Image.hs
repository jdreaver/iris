{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Visual to show an image overlaid on a rectangle

module Iris.Visuals.Image
       ( ImageItem (..)
       , ImageSpec (..)
       , ImageVerts
       , U.readTexture
       , imageInit
       , imageFromFile
       , makeImage
       , drawImage
       ) where

import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V
import qualified Graphics.GLUtil as U
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L

import           Iris.Draw
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

-- | Specification for an ImageItem. This is used to construct the buffers for
-- an image.
data ImageSpec = ImageSpec GL.TextureObject ImageVerts
               deriving (Show)

-- | Vertexes for an ImageItem. These are specified as 4 3D points, ordered
-- counter-clockwise.
type ImageVerts = V.Vector (L.V3 GL.GLfloat)
type ImageFaces = V.Vector (L.V3 GL.GLint)


imageInit :: ImageSpec -> IO DrawNode
imageInit spec =
  do item <- makeImage spec
     return $ DrawNode (drawImage item)

makeImage :: ImageSpec -> IO ImageItem
makeImage (ImageSpec to verts) =
  do prog <- U.simpleShaderProgramBS vsSource fsSource
     vbo <- U.fromSource GL.ArrayBuffer verts
     fbo <- U.fromSource GL.ElementArrayBuffer faces
     tbo <- U.fromSource GL.ArrayBuffer texCoords
     return $ ImageItem prog to vbo fbo tbo verts faces

imageFromFile :: FilePath -> IO (Either String GL.TextureObject)
imageFromFile filePath =
  do to <- U.readTexture filePath
     case to of
       (Left err)  -> return (Left err)
       (Right to') -> do GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
                         U.texture2DWrap $= (GL.Mirrored, GL.ClampToEdge)
                         return $ Right to'

drawImage :: ImageItem -> DrawFunc
drawImage (ImageItem prog to vbo fbo tbo _ fs) (DrawData t _) =
  do enableProgram prog

     enableAttrib prog "coord3d"
     bindVertexBuffer prog "coord3d" vbo 3
     bindElementBuffer fbo

     enableAttrib prog "texcoord"
     bindVertexBuffer prog "texcoord" tbo 2

     setUniform prog "mvp" t

     U.withTextures2D [to] $
       do setUniform prog "mytexture" (0 :: GL.GLint)
          U.drawIndexedTris (fromIntegral $ V.length fs)

     disableAttrib prog "coord3d"
     disableAttrib prog "texcoord"


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
           , "    vec2 coords = vec2(f_texcoord.x, 1.0 - f_texcoord.y);"
           , "    gl_FragColor = texture2D(mytexture, coords);"
           , "}"
           ]


faces :: ImageFaces
faces = [ L.V3 0 1 2
        , L.V3 2 3 0
        ]


texCoords :: V.Vector (L.V2 GL.GLfloat)
texCoords = [ L.V2 0 0
            , L.V2 1 0
            , L.V2 1 1
            , L.V2 0 1
            ]
