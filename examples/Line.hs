{-# LANGUAGE MultiWayIf, OverloadedStrings #-}

-- | This is simply a test to make sure things work until I can come up with
-- some real examples.

module Main where

import           Control.Concurrent.STM
import qualified Linear as L
import           Reactive.Banana
import           Reactive.Banana.Frameworks

import qualified Iris.Backends as W
import           Iris.Camera
import           Iris.Line
import           Iris.SceneGraph
import           Iris.Transformation
import           Iris.Triangle

main :: IO ()
main =
  do win <- W.initGLFW "Line Plot" (640, 480)

     cameraState <- newTVarIO $ panZoomCamera { center = L.V2 1 2
                                              , width = 10
                                              , height = 7
                                              }

     line <- lineItem lineVerts (L.V3 0.2 0.5 1)
     tri <- triangleItem triVerts (L.V3 0.2 1 0.1)

     network <- compile $ mouseNetwork cameraState win
     actuate network

     let items = Collection [ Drawable line
                            , Drawable tri
                            , Transform (translation (L.V3 (-1) 1 0)) (Drawable tri)]
         root = cameraNode cameraState items

     W.drawLoop (drawScene win root) win


lineVerts :: LineVertices
lineVerts = [ L.V2 1 1
            , L.V2 1 2
            , L.V2 2 2
            ]

triVerts :: TriangleVertices
triVerts = [ L.V2 0 0
           , L.V2 0 1
           , L.V2 1 0
           ]
