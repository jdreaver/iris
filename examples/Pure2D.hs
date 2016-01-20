{-# LANGUAGE OverloadedLists #-}

-- | Example using the pure scene graph to make a static scene.

module Main where

import qualified Linear as L

import qualified Iris.Backends.GLFW as W
import           Iris.Camera
import           Iris.SceneGraph
import           Iris.Visuals.Line


main :: IO ()
main =
  do win <- W.makeWindow "Line Plot" (640, 480)
     canvas <- W.initGLFW win

     lineNode <- lineInit $ LineSpec lineVerts (L.V3 0.2 0.5 1)

     let cam = panZoomCamera { center = L.V2 1 2 , width = 10 , height = 7 }
         camNode = transNode (cameraTrans cam) lineNode
         scene = sceneRoot canvas camNode

     W.mainLoop' win (drawGraph scene)


lineVerts :: LineVertices
lineVerts = [ L.V3 0 0 0
            , L.V3 0 1 0
            , L.V3 1 1 0
            , L.V3 1 0 0
            , L.V3 0 0 0
            ]
