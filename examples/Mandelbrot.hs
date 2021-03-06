{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Mandelbrot set visualization example. This example also shows how modular
-- the camera system is. We don't actually use the camera transformation
-- matrix, because all of the computation is done in the shader. However, we
-- can still take advantage of the PanZoom interaction functions.

module Main where

import qualified Data.ByteString as BS
import qualified Linear as L
import qualified Graphics.Rendering.OpenGL as GL

import           Iris.Backends
import qualified Iris.Backends.GLFW as W
import           Iris.Camera.PanZoom
import           Iris.OpenGL
import           Iris.Reactive
import           Iris.SceneGraph
import           Iris.Visuals.Mesh


main :: IO ()
main =
  do win <- W.makeWindow "Mandelbrot set" (640, 640)
     canvas <- W.initGLFW win

     network <- compile $ makeNetwork canvas
     actuate network

     W.mainLoop canvas


makeNetwork :: (Canvas a) => a -> MomentIO ()
makeNetwork canvas =
  do (CanvasEvents mousePosO mouseButtonE mouseScrollE viewportO _ _) <- makeEvents canvas
     let cam = panZoomCamera { panZoomCenter = L.V2 (-0.5) 0
                             , panZoomWidth  = 3
                             , panZoomHeight = 3
                             }
         (Observable viewportB _) = viewportO
     camB <- panZoomB cam mousePosO mouseButtonE viewportB mouseScrollE
     let dataB = fromPanZoom <$> camB <*> viewportB
     item <- liftIO $ mandelbrotInit dataB
     makeScene canvas item

data MandelbrotData = MandelbrotData
  { mandelbrotCenter     :: !(L.V2 GL.GLfloat)
  , mandelbrotResolution :: !(L.V2 GL.GLfloat)
  , mandelbrotScale      :: !(L.V2 GL.GLfloat)
  , mandelbrotIters      :: !GL.GLint
  } deriving (Show)

fromPanZoom :: PanZoomCamera -> Viewport -> MandelbrotData
fromPanZoom (PanZoomCamera center w h _) (Viewport _ (GL.Size rw rh)) =
  MandelbrotData center res (L.V2 w h) 300
  where res = L.V2 (fromIntegral rw) (fromIntegral rh)

data MandelbrotItem = MandelbrotItem ShaderProgram MeshDataBuffer MandelbrotData

meshFaceVerts :: MeshFaceVertices
meshFaceVerts = [ L.V3 (-1) (-1) 0
                , L.V3   1  (-1) 0
                , L.V3   1    1  0
                , L.V3 (-1)   1  0
                ]

meshFaceIndices :: MeshFaceIndices
meshFaceIndices = [ L.V3 0 1 2
                  , L.V3 2 3 0
                  ]

vertSource :: BS.ByteString
vertSource = BS.intercalate "\n"
  [ "attribute vec3 coord3d;"
  , ""
  , "void main()"
  , "{"
  , "    gl_Position = vec4(coord3d, 1.0);"
  , "}"
  ]

fragSource :: BS.ByteString
fragSource = BS.intercalate "\n"
  [ "uniform vec2 resolution;"
  , "uniform vec2 center;"
  , "uniform vec2 scale;"
  , "uniform int iter;"
  , ""
  , "// Jet color scheme"
  , "vec4 color_scheme(float x) {"
  , "    vec3 a, b;"
  , "    float c;"
  , "    if (x < 0.34) {"
  , "        a = vec3(0, 0, 0.5);"
  , "        b = vec3(0, 0.8, 0.95);"
  , "        c = (x - 0.0) / (0.34 - 0.0);"
  , "    } else if (x < 0.64) {"
  , "        a = vec3(0, 0.8, 0.95);"
  , "        b = vec3(0.85, 1, 0.04);"
  , "        c = (x - 0.34) / (0.64 - 0.34);"
  , "    } else if (x < 0.89) {"
  , "        a = vec3(0.85, 1, 0.04);"
  , "        b = vec3(0.96, 0.7, 0);"
  , "        c = (x - 0.64) / (0.89 - 0.64);"
  , "    } else {"
  , "        a = vec3(0.96, 0.7, 0);"
  , "        b = vec3(0.5, 0, 0);"
  , "        c = (x - 0.89) / (1.0 - 0.89);"
  , "    }"
  , "    return vec4(mix(a, b, c), 1.0);"
  , "}"
  , ""
  , "void main() {"
  , "    vec2 z, c;"
  , ""
  , "    // Recover coordinates from pixel coordinates"
  , "    c.x = (gl_FragCoord.x / resolution.x - 0.5) * scale.x + center.x;"
  , "    c.y = (gl_FragCoord.y / resolution.y - 0.5) * scale.y + center.y;"
  , ""
  , "    // Main Mandelbrot computation"
  , "    int i;"
  , "    z = c;"
  , "    for(i = 0; i < iter; i++) {"
  , "        float x = (z.x * z.x - z.y * z.y) + c.x;"
  , "        float y = (z.y * z.x + z.x * z.y) + c.y;"
  , ""
  , "        if((x * x + y * y) > 4.0) break;"
  , "        z.x = x;"
  , "        z.y = y;"
  , "    }"
  , ""
  , "    // Convert iterations to color"
  , "    float color = 1.0 - float(i) / float(iter);"
  , "    gl_FragColor = color_scheme(color);"
  , ""
  , "}"
  ]


mandelbrotInit :: Behavior MandelbrotData -> IO (Behavior DrawNode)
mandelbrotInit mandelDataB =
  do prog <- simpleShaderProgramBS vertSource fragSource
     mbuf <- meshBuffer (Faces meshFaceVerts meshFaceIndices)
     let item = MandelbrotItem prog mbuf <$> mandelDataB
     return $ DrawNode <$> (drawMandelbrot <$> item)

drawMandelbrot :: MandelbrotItem -> DrawFunc
drawMandelbrot (MandelbrotItem prog meshData
                (MandelbrotData cent res scale iter)
               ) _ =
  do enableProgram prog
     enableAttrib prog "coord3d"
     bindMeshData prog meshData
     setUniform prog "center" cent
     setUniform prog "resolution" res
     setUniform prog "scale" scale
     setUniform prog "iter" iter
     drawMeshData meshData
     disableAttrib prog "coord3d"
