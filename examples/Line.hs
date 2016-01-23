{-# LANGUAGE OverloadedLists #-}

-- | This is simply a test to make sure things work until I can come up with
-- some real examples.

module Main where

import qualified Linear as L

import qualified Iris.Backends.GLFW as W
import           Iris.Camera
import           Iris.Reactive
import           Iris.SceneGraph
import           Iris.Transformation
import           Iris.Visuals

main :: IO ()
main =
  do win <- W.makeWindow "Line Plot" (640, 640)
     canvas <- W.initGLFW win

     line <- lineInit $ lineSpec
             { lineSpecVertices = lineVerts
             , lineSpecColors   = L.V3 0.2 0.5 1
             }
     mesh <- meshInit $ meshSpec
             { meshSpecData   = Vertexes meshVerts
             , meshSpecColors = ConstantMeshColor (L.V3 0.2 1 0.1)
             }
     mesh2 <- meshInit $ meshSpec
              { meshSpecData   = Faces meshFaceVerts meshFaceIndices
              , meshSpecColors = ConstantMeshColor (L.V3 1 0.2 0.1)
              }
     mesh3 <- meshInit $ meshSpec
              { meshSpecData   = Vertexes meshVerts ,
                meshSpecColors = VectorMeshColor meshVertColors
              }

     let items = groupNode
                 [ line
                 , mesh
                 , transNode (translation (L.V3 2 2 0)) mesh2
                 , transNode (translation (L.V3 (-1) 1 0)) mesh
                 , transNode (translation (L.V3 (-2) 3 0)) mesh3
                 ]
         cam = panZoomCamera { center = L.V2 1 2 , width = 10 , height = 7 }

     network <- compile $ makeScene canvas (pure items) (Just cam)
     actuate network

     W.mainLoop canvas


lineVerts :: LineVertices
lineVerts = [ L.V3 1 1 0
            , L.V3 1 2 0
            , L.V3 2 2 0
            ]

meshVerts :: MeshVertices
meshVerts = [ L.V3 (L.V3 0 0 0) (L.V3 1 1 0) (L.V3 0 1 0)
            , L.V3 (L.V3 0 0 0) (L.V3 1 0 0) (L.V3 1 1 0)
            ]

meshVertColors :: MeshVectorColor
meshVertColors = [ L.V3 0 1 0
                 , L.V3 1 0 0
                 , L.V3 0 0 1
                 , L.V3 1 1 0
                 , L.V3 0 1 1
                 , L.V3 1 0 1
                 ]

meshFaceVerts :: MeshFaceVertices
meshFaceVerts = [ L.V3 0 0 0
                , L.V3 1 1 0
                , L.V3 1 2 0
                , L.V3 0 1 0
                ]

meshFaceIndices :: MeshFaceIndices
meshFaceIndices = [ L.V3 0 1 2
                  , L.V3 2 3 0
                  ]
