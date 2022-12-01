module Tigris.OpenGL.PNG where

-- juicy pixels
import qualified Codec.Picture as J

-- opengl
import qualified Graphics.Rendering.OpenGL as GL

-- vector
import qualified Data.Vector.Storable as V

-- base
import Foreign.Ptr

createTextureFromPNG :: FilePath -> IO (Either String GL.TextureObject)
createTextureFromPNG filePath = do
  ei <- J.readPng filePath
  case ei of
    Left str -> pure . Left $ str
    Right dynamicImage -> do
      case dynamicImage of
        J.ImageRGBA8 ipng -> do
          tex <- (GL.genObjectName :: IO GL.TextureObject)
          GL.textureBinding GL.Texture2D GL.$= Just tex
          V.unsafeWith (J.imageData ipng) $
            GL.texImage2D GL.Texture2D
                          GL.NoProxy
                          0
                          GL.RGBA8
                          (GL.TextureSize2D
                            (fromIntegral . J.imageWidth $ ipng)
                            (fromIntegral . J.imageHeight $ ipng))
                          0 .
            GL.PixelData GL.RGBA GL.UnsignedByte . castPtr
          GL.generateMipmap' GL.Texture2D
          GL.textureFilter GL.Texture2D GL.$= ((GL.Nearest, Just GL.Nearest), GL.Nearest)
          GL.textureBinding GL.Texture2D GL.$= Nothing
          pure $ Right tex
        _ -> pure . Left $ "Unrecognized format."

            
