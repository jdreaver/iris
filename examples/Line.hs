{-# LANGUAGE MultiWayIf, OverloadedStrings #-}

-- | This is simply a test to make sure things work until I can come up with
-- some real examples.

module Main where

import           Control.Lens
import qualified Linear as L
import           Reactive.Banana
import           Reactive.Banana.Frameworks

import qualified Iris.Backends.GLFW as W
import           Iris.Camera
import           Iris.Events
import           Iris.Reactive
import           Iris.SceneGraph
import           Iris.Transformation
import           Iris.Visuals

main :: IO ()
main =
  do win <- W.makeWindow "Line Plot" (640, 480)
     canvas <- W.initGLFW win

     let cam = panZoomCamera { center = L.V2 1 2 , width = 10 , height = 7 }


     network <- compile $ makeNetwork canvas cam
     actuate network

     W.mainLoop canvas


makeNetwork :: W.GLFWCanvas -> PanZoomCamera -> MomentIO ()
makeNetwork canvas cam =
  do events <- W.makeEvents canvas
     (bCam, hPos, hScroll) <- mouseNetwork cam events
     handleEvent [hPos] (events ^. W.mousePosObservable ^. event)
     handleEvent [hScroll] (events ^. W.mouseScrollEvent)

     line <- lineInit $ LineSpec lineVerts (L.V3 0.2 0.5 1)
     tri <- triangleInit $ TriangleSpec triVerts (L.V3 0.2 1 0.1)

     let items = Collection [ VisualNode line
                            , VisualNode tri
                            , Transform (pure $ translation (L.V3 (-1) 1 0))
                              (VisualNode tri)]

     let eDraw = events ^. W.drawEvent
         root = cameraNode bCam items
     makeScene canvas events eDraw root


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
