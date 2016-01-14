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

     char1 <- loadCharacter path 'z' 512
     node1 <- imageInit $ ImageSpec (characterTexture char1) verts
     char2 <- loadCharacter path 'g' 512
     node2 <- imageInit $ ImageSpec (characterTexture char2) verts
     let cam = panZoomCamera { width = 2, height = 2 }
         coll = groupNode
                [ transNode (translation (L.V3 (-0.5) 0 0)) [node1]
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
