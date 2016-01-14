{-# LANGUAGE OverloadedLists #-}

-- | Shows a text item

module Main where

import qualified Linear as L
import           System.Environment (getArgs)

import qualified Iris.Backends.GLFW as W
import           Iris.Camera
import           Iris.Reactive
import           Iris.SceneGraph
import           Iris.Text
import           Iris.Transformation
import           Iris.Visuals.Image


main :: IO ()
main =
  do path <- getFilePath
     win <- W.makeWindow "Image" (640, 480)
     canvas <- W.initGLFW win

     to <- loadCharacter path 'z' 200
     node <- imageInit $ ImageSpec to verts
     to2 <- loadCharacter path 'X' 500
     node2 <- imageInit $ ImageSpec to2 verts
     let cam = panZoomCamera { width = 2, height = 2 }
         coll = groupNode
                [ transNode (translation (L.V3 (-0.5) 0 0)) [node]
                , transNode (translation (L.V3 0.5 0 0)) [node2]
                ]

     network <- compile $ makeScene canvas (pure coll) (Just cam)
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
