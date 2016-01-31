-- | Wrapper around OpenGL error printing functions.

module Iris.OpenGL.Errors
       ( printError
       , throwError
       , GLError (..)
       ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import Data.List (intercalate)
import Data.Typeable (Typeable)
import Graphics.Rendering.OpenGL
import System.IO (hPutStrLn, stderr)

-- | If there is an error in OpenGL, then print to 'stderr'.
printError :: IO ()
printError =
  do errs <- get errors
     mapM_ (hPutStrLn stderr . ("iris OpenGL error: " ++) . show) errs


-- | Throw an exception if there is an OpenGL error.
throwError :: IO ()
throwError = do errs <- get errors
                unless (null errs)
                  (throwIO . GLError . tail $ printGLErrors errs)


-- | An exception type for OpenGL errors.
data GLError = GLError String deriving (Typeable)
instance Exception GLError where
instance Show GLError where
  show (GLError msg) = "GLError " ++ msg

-- |Prefix each of a list of messages with "GL: ".
printGLErrors :: Show a => [a] -> String
printGLErrors = intercalate "\n  GL: " . ("" :) . map show
