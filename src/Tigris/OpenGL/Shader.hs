module Tigris.OpenGL.Shader
  ( loadShaders
  ) where

-- opengl
import qualified Graphics.Rendering.OpenGL as GL

-- bytestring
import qualified Data.ByteString as B

-- base
import Control.Monad
import Control.Exception


check :: (t -> IO ())
      -> (t -> GL.GettableStateVar Bool)
      -> (t -> GL.GettableStateVar String)
      -> String
      -> t
      -> IO ()
check action getStatus getInfoLog message object = do
  action object
  ok <- GL.get (getStatus object)
  unless ok $ do
    infoLog <- GL.get (getInfoLog object)
    fail (message ++ " log: " ++ infoLog)

linkAndCheck :: GL.Program -> IO ()
linkAndCheck = check GL.linkProgram GL.linkStatus GL.programInfoLog "link"

compileAndCheck :: GL.Shader -> IO ()
compileAndCheck = check GL.compileShader GL.compileStatus GL.shaderInfoLog "compile"

loadCompileAttach :: GL.Program -> [(GL.ShaderType, FilePath)] -> IO ()
loadCompileAttach _ [] = return ()
loadCompileAttach program ((shType, path) : rest) =
  GL.createShader shType `bracketOnError` GL.deleteObjectName $ \shader -> do
    src <- B.readFile path
    GL.shaderSourceBS shader GL.$= src
    compileAndCheck shader
    GL.attachShader program shader
    loadCompileAttach program rest

loadShaders :: [(GL.ShaderType, FilePath)] -> IO GL.Program
loadShaders lst =
  GL.createProgram `bracketOnError` GL.deleteObjectName $ \program -> do
    loadCompileAttach program lst
    linkAndCheck program
    return program
