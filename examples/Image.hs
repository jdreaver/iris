{-# LANGUAGE OverloadedLists #-}

-- | Show a simple image.

module Main where

import qualified Linear as L
import           System.Environment (getArgs)

import qualified Iris.Backends.GLFW as W
import           Iris.Camera
import           Iris.Reactive
import           Iris.SceneGraph
import           Iris.Visuals.Image


main :: IO ()
main =
  do path <- getFilePath
     win <- W.makeWindow "Image" (640, 640)
     canvas <- W.initGLFW win

     to <- imageFromFile path
     case to of
       (Left err) -> putStrLn $ "Error: " ++ err
       (Right to') -> do node <- imageInit (ImageSpec to' verts)
                         main' canvas node

main' :: W.GLFWCanvas -> DrawNode -> IO ()
main' canvas node =
  do let cam = arcBallCamera { arcBallWidth     = 2
                             , arcBallAzimuth   = 30 * pi / 180
                             , arcBallElevation = 30 * pi / 180
                             }

     network <- compile $ makeScene canvas (pure node) (Just cam)
     actuate network

     W.mainLoop canvas


getFilePath :: IO FilePath
getFilePath =
  do args <- getArgs
     case args of
       (x:_) -> return x
       _     -> fail "Please supply image path"


verts :: ImageVerts
verts = [ L.V3 (-0.5) (-0.5) 0
        , L.V3   0.5  (-0.5) 0
        , L.V3   0.5    0.5  0
        , L.V3 (-0.5)   0.5  0
        ]
