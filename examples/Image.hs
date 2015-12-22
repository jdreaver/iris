-- | Show a simple image.

module Main where

import           System.Environment (getArgs)

import qualified Iris.Backends.GLFW as W
import           Iris.Camera
import           Iris.Reactive
import           Iris.SceneGraph
import           Iris.Visuals.Image


main :: IO ()
main =
  do path <- getFilePath
     spec <- imageFromFile path
     case spec of
       (Left err) -> putStrLn $ "Error: " ++ err
       (Right spec') -> do win <- W.makeWindow "Image" (640, 480)
                           canvas <- W.initGLFW win
                           node <- imageInit spec'
                           main' canvas node

main' :: W.GLFWCanvas -> DrawNode -> IO ()
main' canvas node =
  do let cam = panZoomCamera { width = 3 , height = 3 }

     network <- compile $ makeScene canvas (pure node) (Just cam)
     actuate network

     W.mainLoop canvas


getFilePath :: IO FilePath
getFilePath =
  do args <- getArgs
     case args of
       []    -> fail "Please supply image path"
       (x:_) -> return x
