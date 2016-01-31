-- | Convenient wrapper around OpenGL shader programs so we can easily set
-- uniform attributes. This is heavily inspired by Anthony Cowley's GLUtil
-- package.

module Iris.OpenGL.ShaderProgram
       ( ShaderProgram
       , enableProgram
       , simpleShaderProgramBS
       , loadShaderProgramBS
       , setUniform
       , getUniform
       , setAttrib
       , getAttrib
       , enableAttrib
       , disableAttrib
       , withAttrib
       ) where

import           Control.Monad (unless, void)
import qualified Data.ByteString as BS
import           Data.List (isSuffixOf)
import           Data.Map.Strict (Map, fromList, lookup)
import           Graphics.Rendering.OpenGL
import           Prelude hiding (lookup)

import           Iris.OpenGL.AsUniform (AsUniform (..))
import           Iris.OpenGL.Errors (printError, throwError)

-- | Wrapper around an OpenGL 'Program', with uniform and attrib locations
-- stored in maps.
data ShaderProgram = ShaderProgram
  { shaderAttribs  :: Map String (AttribLocation, VariableType)
  , shaderUniforms :: Map String (UniformLocation, VariableType)
  , shaderProgram  :: Program
  } deriving (Show)

-- | Sets the current OpenGL shader program.
enableProgram :: ShaderProgram -> IO ()
enableProgram prog = currentProgram $= Just (shaderProgram prog)

-- | Load a 'ShaderProgram' from vertex and fragment shader source
-- strings. The active attributes and uniforms in the linked program
-- are recorded in the 'ShaderProgram'.
simpleShaderProgramBS :: BS.ByteString -> BS.ByteString -> IO ShaderProgram
simpleShaderProgramBS vsrc fsrc =
  loadShaderProgramBS [(VertexShader, vsrc), (FragmentShader, fsrc)]

loadShaderProgramBS :: [(ShaderType, BS.ByteString)]
                    -> IO ShaderProgram
loadShaderProgramBS sources =
  do p <- mapM (uncurry $ loadShaderBS "BS") sources >>= linkShaderProgram
     throwError
     attrs <- getActiveAttribs p
     unifs <- getActiveUniforms p
     return $ ShaderProgram (fromList attrs) (fromList unifs) p

-- | @loadShaderBS fileName shaderType src@ loads a shader from source code,
-- @src@. The file name is used only for error reporting. (Function taken from
-- GLUtil).
loadShaderBS :: FilePath -> ShaderType -> BS.ByteString -> IO Shader
loadShaderBS filePath st src = do
  shader <- createShader st
  shaderSourceBS shader $= src
  compileShader shader
  printError
  ok <- get (compileStatus shader)
  infoLog <- get (shaderInfoLog shader)
  unless (null infoLog || infoLog == "\NUL")
         (mapM_ putStrLn
                ["Shader info log for '" ++ filePath ++ "':", infoLog, ""])
  unless ok $ do
    deleteObjectName shader
    ioError (userError "shader compilation failed")
  return shader

-- | Link shaders into a 'Program'.
linkShaderProgram :: [Shader] -> IO Program
linkShaderProgram shaders =
  do p <- createProgram
     mapM_ (attachShader p) shaders
     linkProgram p
     return p

-- | Get all attributes used by a program. Note that unused parameters may be
-- elided by the compiler, and so will not be considered as active.
getActiveAttribs :: Program -> IO [(String, (AttribLocation, VariableType))]
getActiveAttribs p =
  do attribs <- get (activeAttribs p)
     mapM (getActiveAux (attribLocation p)) attribs

-- | Get all uniforms used by a program. Note that unused parameters may be
-- elided by the compiler, and so will not be considered as active.
getActiveUniforms :: Program -> IO [(String, (UniformLocation, VariableType))]
getActiveUniforms p =
  do uniforms <- get (activeUniforms p)
     mapM (getActiveAux (uniformLocation p) . on3 trimArray) uniforms
       where on3 f (a,b,c) = (a, b, f c)
             -- Used to trim the "[0]" that is sometimes appended to uniform
             -- locations.
             trimArray n = if "[0]" `isSuffixOf` n then take (length n - 3) n else n

-- | Used to get transform a uniform or attrib location into our tuple form.
getActiveAux :: (HasGetter g a)
             => (String -> g)
             -> (GLint, VariableType, String)
             -> IO (String, (a, VariableType))
getActiveAux f (_, t, name) =
  do l <- get (f name)
     return (name, (l, t))


-- | Set a named uniform parameter associated with a particular shader
-- program.
setUniform :: AsUniform a => ShaderProgram -> String -> a -> IO ()
setUniform sp name = maybe (const (void (putStrLn warn)))
                           (flip asUniform . fst)
                           (lookup name $ shaderUniforms sp)
  where warn = "WARNING: uniform " ++ name ++ " is not active"

-- | Get the 'UniformLocation' associated with a named uniform
-- parameter.
getUniform :: ShaderProgram -> String -> UniformLocation
getUniform sp n = maybe (error msg) fst . lookup n $ shaderUniforms sp
  where msg = "Uniform " ++ show n ++ " is not active"

-- | Set a named vertex attribute's 'IntegerHandling' and
-- 'VertexArrayDescriptor'.
setAttrib :: ShaderProgram -> String ->
             IntegerHandling -> VertexArrayDescriptor a -> IO ()
setAttrib sp name = maybe (\_ _ -> void (putStrLn warn))
                          (\(a,_) -> let vap = vertexAttribPointer a
                                     in \ih vad -> (($= (ih, vad)) vap))
                          (lookup name $ shaderAttribs sp)
  where warn = "WARNING: attrib " ++ name ++ " is not active"

-- | Get the 'AttribLocation' associated with a named vertex
-- attribute.
getAttrib :: ShaderProgram -> String -> AttribLocation
getAttrib sp n = maybe (error msg) fst . lookup n $ shaderAttribs sp
  where msg = "Attrib " ++ show n ++ " is not active"

-- | Enable a named vertex attribute.
enableAttrib :: ShaderProgram -> String -> IO ()
enableAttrib sp name = maybe (return ())
                             (($= Enabled) . vertexAttribArray . fst)
                             (lookup name $ shaderAttribs sp)

-- | Disable a named vertex attribute.
disableAttrib :: ShaderProgram -> String -> IO ()
disableAttrib prog name = vertexAttribArray (getAttrib prog name) $= Disabled


-- | Executes the IO action by first enabling an attribute, then performing the
-- action, then disabling the attribute.
withAttrib :: ShaderProgram -> String -> IO a -> IO a
withAttrib prog name f =
  do enableAttrib prog name
     result <- f
     disableAttrib prog name
     return result
