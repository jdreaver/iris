-- | Example of a simple 3D cube and an arcball camera. The cube vertexes and
-- colors were taken from
-- https://en.wikibooks.org/wiki/OpenGL_Programming/Modern_OpenGL_Tutorial_05

module Main where

import qualified Data.Vector.Storable as V
import qualified Linear as L

import qualified Iris.Backends.GLFW as W
import           Iris.Camera
import           Iris.Reactive
import           Iris.SceneGraph
import           Iris.Visuals

main :: IO ()
main =
  do win <- W.makeWindow "3D Cube" (640, 480)
     canvas <- W.initGLFW win

     cube <- meshInit $ meshSpec
             { meshSpecData   = Faces cubeVerts cubeIndices
             , meshSpecColors = VectorMeshColor cubeColors
             }

     let cam  = arcBallCamera { arcBallWidth     = 6
                              , arcBallAzimuth   = 30 * pi / 180
                              , arcBallElevation = 30 * pi / 180
                              }

     network <- compile $ makeScene canvas (VisualNode cube) (Just cam)
     actuate network

     W.mainLoop canvas


cubeVerts :: MeshFaceVertices
cubeVerts = V.fromList [-- front
                         L.V3 (-1) (-1) 1
                       , L.V3 1    (-1) 1
                       , L.V3 1    1    1
                       , L.V3 (-1) 1    1
                         -- back
                       , L.V3 (-1) (-1) (-1)
                       , L.V3 1    (-1) (-1)
                       , L.V3 1    1    (-1)
                       , L.V3 (-1) 1    (-1)
                       ]

cubeIndices :: MeshFaceIndices
cubeIndices = V.fromList [-- front
                           L.V3 0 1 2
                         , L.V3 2 3 0
                           -- top
                         , L.V3 3 2 6
                         , L.V3 6 7 3
                           -- back
                         , L.V3 7 6 5
                         , L.V3 5 4 7
                           -- bottom
                         , L.V3 4 5 1
                         , L.V3 1 0 4
                           -- left
                         , L.V3 4 0 3
                         , L.V3 3 7 4
                           -- right
                         , L.V3 1 5 6
                         , L.V3 6 2 1
                         ]

cubeColors :: MeshVectorColor
cubeColors = V.fromList [-- front colors
                          L.V3 1 0 0
                        , L.V3 0 1 0
                        , L.V3 0 0 1
                        , L.V3 1 1 1
                         -- back colors
                        , L.V3 1 1 0
                        , L.V3 0 1 1
                        , L.V3 1 0 1
                        , L.V3 0 0 0
                        ]
