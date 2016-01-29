{-# LANGUAGE OverloadedLists #-}

-- | Shows a text item

module Main where

import qualified Linear as L
import           System.Environment (getArgs)

import qualified Iris.Backends.GLFW as W
import           Iris.Camera
import           Iris.Reactive
import           Iris.SceneGraph
import           Iris.Transformation
import           Iris.Visuals


main :: IO ()
main =
  do path <- getFilePath
     win <- W.makeWindow "Text" (640, 640)
     canvas <- W.initGLFW win

     node <- textInit $ TextSpec "Hello! AVWY" path (L.V2 0 0) 1 512
     let cam = panZoomCamera { panZoomWidth  = 5
                             , panZoomHeight = 5 }
         node' = transNode (translation (L.V3 (-1) 0 0)) node

     network <- compile $ sceneWithCamera canvas (pure node') cam
     actuate network

     W.mainLoop canvas


getFilePath :: IO FilePath
getFilePath =
  do args <- getArgs
     case args of
       []    -> fail "Please supply a file path to a .ttf file"
       (x:_) -> return x


verts :: ImageVerts
verts = [ L.V3 (-0.5) (-0.5) 0
        , L.V3   0.5  (-0.5) 0
        , L.V3   0.5    0.5  0
        , L.V3 (-0.5)   0.5  0
        ]
