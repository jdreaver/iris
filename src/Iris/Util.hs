{-# LANGUAGE OverloadedLists #-}

-- | Miscellaneous utility functions

module Iris.Util where

import qualified Linear as L

import           Iris.SceneGraph
import           Iris.Visuals

makeCube :: IO DrawNode
makeCube = meshInit $ meshSpec
             { meshSpecData   = Faces cubeVerts cubeIndices
             , meshSpecColors = VectorMeshColor cubeColors
             }


cubeVerts :: MeshFaceVertices
cubeVerts = [-- front
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
cubeIndices = [-- front
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
cubeColors = [-- front colors
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
