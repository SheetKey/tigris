{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where


-- tigris
import Tigris

-- opengl
import qualified Graphics.Rendering.OpenGL as GL

-- sdl2
import qualified SDL

-- apecs
import Apecs

-- vector
import qualified Data.Vector.Storable as V
import qualified Data.Vector as VV

-- containers
import qualified Data.IntMap.Strict as M

-- linear
import Linear

-- base
import Data.Foldable (traverse_)
import Foreign.Ptr
import Control.Concurrent
import Control.Monad (forM, forM_)

-- text
import Data.Text hiding (zip)

-- rhine
import FRP.Rhine hiding (get)

-- random
import System.Random (randomRIO)



main :: IO ()
main = initAndRun "Game Demo" gameLoop'''
--main = do 
--  g <- wfc (VV.fromList [tile1, tile2, tile3, tile4]) (2, 2) Nothing
--  print g
----             n e s w
--tile1 = Tile 1 1 2 1 1 1
--tile2 = Tile 2 1 1 1 2 1
--tile3 = Tile 3 2 2 1 2 1
--tile4 = Tile 4 1 2 2 2 1

-- used in funmap: propably move into the library
setYV3 :: GL.GLfloat -> V3 GL.GLfloat -> V3 GL.GLfloat
setYV3 y (V3 x _ z) = V3 x y z

testFuncMap :: MonadIO m => FuncMap (V3 GL.GLfloat, V3 GL.GLfloat) m ()
testFuncMap = M.fromList [ (1, \_ xzPos -> liftIO $ print xzPos)
                         , (2
                           , \ety (_, xzPos) -> do
                               Position (V4 _ pos _ _) <- get (Entity ety)
                               ShootOffset (xzOffset, yOffset) <- get (Entity ety)
                               set (Entity ety) (Shoot (setYV3 yOffset $ pos + xzOffset, setYV3 yOffset xzPos))
                           )
                         ]


clsfLoop :: MonadIO m => ClSFS m (HoistClock IO (SystemT World m) (Millisecond 16)) () ()
clsfLoop =
  --aalthandleEvent
  eventHandler
  >>> useLeftClick testFuncMap
  >>> setPlayerVelocity
  >>> setPosition
  >>> follow
  >>> shoot
  >>> Tigris.rotate
  >>> model
  >>> customView
  >>> rotAngleToMouse
  >>> incFrame
  >>> uv
  -- >>> mousePosition' >>> mouseCoord >>> arr (\_ -> ())

rhineLoop'
  :: World
  -> RhineS IO (ParClockS IO (ParClockS IO (HoistClock IO (SystemT World IO) (Millisecond 16))
                                            WindowResizeClock
                              )
               (HoistClock IO (SystemT World IO) (Millisecond 16))
               )
  () ()
rhineLoop' world =
  (clsfLoop @@ (HoistClock waitClock liftIO) ||@ (concurrentlySystem world) @||
     (projection @@ WindowResizeClock)
  )
  ||@ (concurrentlySystem world) @||
  (altDraw) @@ (HoistClock waitClock liftIO)

rhineLoop
  :: World
  -> RhineS IO (ParClockS IO 
                (ParClockS IO (SeqClockS IO
                                (HoistClock IO (SystemT World IO) Busy)
                                (HoistClock IO (SystemT World IO) Busy)
                              )
                              (ParClockS IO (HoistClock IO (SystemT World IO) (Millisecond 16))
                                            WindowResizeClock
                              )
                )
                (HoistClock IO (SystemT World IO) (Millisecond 16))
               )
  () ()
rhineLoop world =
  (althandleEvent world ||@ (concurrentlySystem world) @||
    (clsfLoop @@ (HoistClock waitClock liftIO) ||@ (concurrentlySystem world) @||
     (projection @@ WindowResizeClock)
    )
  )
  ||@ (concurrentlySystem world) @||
  (altDraw) @@ (HoistClock waitClock liftIO)



gameLoop' :: World -> SystemT' IO ()
gameLoop' = mkGameLoop
  (flow $ (clsfLoop >>> altDraw)
   @@ ((HoistClock waitClock liftIO) :: HoistClock IO (SystemT World IO) (Millisecond 16)))

gameLoop''' :: World -> SystemT' IO ()
gameLoop''' world = mkGameLoop
  (flow $ ((clsfLoop >>> altDraw)
            @@ ((HoistClock waitClock liftIO) :: HoistClock IO (SystemT World IO) (Millisecond 16)))
   ||@ (concurrentlySystem world) @||
   (projection @@ WindowResizeClock))
  world
                    

--  do
--  GLBuffers (vao,_,_, program) <- get global
--
--  liftIO $ do 
--    GL.currentProgram GL.$= Just program
--    Right texture0001 <- createTextureFromPNG "./sprites/Sheet-1.png"
--    GL.activeTexture GL.$= GL.TextureUnit 0
--    GL.textureBinding GL.Texture2D GL.$= Just texture0001
--    locTexture <- GL.get . GL.uniformLocation program $ "Texture"
--    GL.uniform locTexture GL.$= GL.TextureUnit 0
--    GL.currentProgram GL.$= Nothing
--
--  loadNewGrid "data/sheet-1.db"
--
--  Entity _id <- player
--  followPlayer _id
--
--  _projection $ V2 800 600
--
--  --flow $ rhineLoop' world
--  flow $
--    (clsfLoop >>> altDraw)
--    @@ ((HoistClock waitClock liftIO) :: HoistClock IO (SystemT World IO) (Millisecond 16))


player :: SystemT' IO Entity
player = 
  let p = V3 0 0 0
  in newEntity ( Player
                , Size (V4 (V3 (-16) 32 0) (V3 16 32 0) (V3 16 0 0) (V3 (-16) 0 0))
                , Position (V4 p p p p)
                , Speed 250
                , SpriteSheet 1 (4096-34) 0 0 (34 * 4) 34 34 1 2 0
                , PVelocity (Z, Z)
                , WantLeftClick 2
                , ( ProjStats 5 1 (Speed 100)
                  , ShootOffset ((V3 0 0 0), 16)
                  )
                )
followPlayer :: Int -> SystemT' IO ()
followPlayer _id =
  newEntity_ ( Size (V4 (V3 (-16) 32 0) (V3 16 32 0) (V3 16 0 0) (V3 (-16) 0 0))
             , SpriteSheet 1 (4096) 0 0 (34 * 4) 34 34 1 2 0
             , Follows _id (V3 32 0 0)
             , Rotation 0 0 0 (2, 2) 
             , RToMouse
             )

tree :: Position -> SystemT' IO ()
tree pos =
  newEntity_ (Size (V4 (V3 (-115.5) 264 0) (V3 115.5 264 0) (V3 115.5 0 0) (V3 (-115.5) 0 0))
             , SpriteSheet 1 4096 1190 4096 1190 238 272 1 1000000000 0
             , pos
             )

trees :: SystemT' IO ()
trees = do
  randListX <- forM [1 .. 10] $ \_ -> randomRIO (0, 1024)
  randListZ <- forM [1 .. 10] $ \_ -> randomRIO (-1024, 0)
  let randList = zip randListX randListZ
      tList = (flip fmap) randList $ \(x, z) -> let p = V3 x 0 z in Position (V4 p p p p)
  forM_ tList tree 
                   
  
mkGameLoop :: SystemT' IO () -> World -> SystemT' IO ()
mkGameLoop loop world = do
  GLBuffers (vao,_,_, program) <- get global

  liftIO $ do 
    GL.currentProgram GL.$= Just program
    Right texture0001 <- createTextureFromPNG "./sprites/Sheet-1.png"
    GL.activeTexture GL.$= GL.TextureUnit 0
    GL.textureBinding GL.Texture2D GL.$= Just texture0001
    locTexture <- GL.get . GL.uniformLocation program $ "Texture"
    GL.uniform locTexture GL.$= GL.TextureUnit 0
    GL.currentProgram GL.$= Nothing

  loadNewGrid "data/sheet-1.db"

  Entity _id <- player
  followPlayer _id
  trees

  _projection $ V2 800 600

  loop
















  

vertices :: V.Vector GL.GLfloat
vertices = V.fromList
  [ -- positions             -- texture
    -0.25,  0.51, 0.0, 1.0,  0.0, 1.0
  ,  0.25,  0.51, 0.0, 1.0,  1.0/5, 1.0
  ,  0.25,  0.01, 0.0, 1.0,  1.0/5, 0.0
  , -0.25,  0.01, 0.0, 1.0,  0.0, 0.0

  , -5.0,   0.0,  5.0, 1.0,  0.0, 1.0
  ,  5.0 ,  0.0,  5.0, 1.0,  1.0/5, 1.0
  ,  5.0 ,  0.0, -5.0, 1.0,  1.0/5, 0.0
  , -5.0 ,  0.0, -5.0, 1.0,  0.0, 0.0
  ]

indices :: V.Vector GL.GLuint
indices = V.fromList
  [
    0, 1, 2, 0, 2, 3
  , 4, 5, 6, 4, 6, 7
  --  4, 5, 6, 7
  --, 10000
  --, 0, 1, 2, 3
  ]

gameLoop :: World -> SystemT' IO ()
gameLoop world = do
  liftIO $ do
    ---------------------------------------------------------------------
    -- opengl settings
    ---------------------------------------------------------------------
    -- opengl blending
    GL.blend GL.$= GL.Enabled
    GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

    -- depth test
    GL.depthFunc GL.$= Just GL.Less

    -- msaa 
    GL.multisample GL.$= GL.Enabled
    ---------------------------------------------------------------------
    ---------------------------------------------------------------------
    
    -- create the vao, vbo, and ebo
    (vao, vbo, ebo) <- bufferInit
    GL.bindVertexArrayObject GL.$= Just vao
    -- load the vertices into the vbo
    bufferDataWithVector vertices GL.ArrayBuffer GL.StaticDraw
    -- load the indices into the ebo
    bufferDataWithVector indices GL.ElementArrayBuffer GL.StaticDraw

    -- create the shader program
    program <- loadShaders [ (GL.VertexShader, "./shaders/default.vert")
                           , (GL.FragmentShader, "./shaders/default.frag")
                           ]
    -- set the current program to be the program
    GL.currentProgram GL.$= Just program
  
    -- -- load the texture
    Right texture0001 <- createTextureFromPNG "./sprites/Sprite-0001.png"

    -- print errors
    traverse_ (putStrLn . show) <$> (GL.get GL.errors)
  
    -- -- activate texture unit 0 and bind texture
    GL.activeTexture GL.$= GL.TextureUnit 0
    GL.textureBinding GL.Texture2D GL.$= Just texture0001
  
    -- -- get the shader uniform locations
    locTexture <- GL.get . GL.uniformLocation program $ "Texture"
    locView <- GL.get . GL.uniformLocation program $ "view"
    locProj <- GL.get . GL.uniformLocation program $ "proj"

    -- -- set the texture to the shader uniform
    GL.uniform locTexture GL.$= GL.TextureUnit 0
    -- -- set the view and proj uniform matrices
    let vm :: M44 GL.GLfloat = viewMatrix (V3 0 0 0) (V3 0 2 5)
        pm :: M44 GL.GLfloat = projectionMatrix 45 800 600
    (toMatrix vm :: IO (GL.GLmatrix GL.GLfloat)) >>= (GL.uniform locView GL.$=) 
    (toMatrix pm :: IO (GL.GLmatrix GL.GLfloat)) >>= (GL.uniform locProj GL.$=) 

    -- set opengl viewport
    GL.viewport GL.$= (GL.Position 0 0, GL.Size 800 600)
    
    -- set the open gl clear color
    GL.clearColor GL.$= GL.Color4 0.4 0.8 1.0 1.0

    -- clear the buffers
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    -- draw the square
    GL.drawElements GL.Triangles (fromIntegral $ V.length indices) GL.UnsignedInt nullPtr

  -- get the sdl window from apecs
  Window window <- get global

  -- sdl present the opengl window
  SDL.glSwapWindow window
  liftIO $ do
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    GL.drawElements GL.Triangles (fromIntegral $ V.length indices) GL.UnsignedInt nullPtr
  SDL.glSwapWindow window
  
  liftIO $ threadDelay 20000

  liftIO $ do
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    GL.drawElements GL.Triangles (fromIntegral $ V.length indices) GL.UnsignedInt nullPtr
  SDL.glSwapWindow window
  
  liftIO $ threadDelay 5000000

gameLoop'' :: World -> SystemT' IO ()
gameLoop'' world = do
  GLBuffers (vao, vbo, ebo, program) <- get global

  liftIO $ do
    GL.currentProgram GL.$= Just program
    Right texture0001 <- createTextureFromPNG "./sprites/Sprite-0001.png"
    GL.activeTexture GL.$= GL.TextureUnit 0
    GL.textureBinding GL.Texture2D GL.$= Just texture0001
    locTexture <- GL.get . GL.uniformLocation program $ "Texture"
    GL.uniform locTexture GL.$= GL.TextureUnit 0
    GL.currentProgram GL.$= Nothing

  let p = V3 0 0 0
      p' = V3 2 0 2
  player
  newEntity_ ( Size (V4 (V3 (-0.25) 0.5 0) (V3 0.25 0.5 0) (V3 0.25 0 0) (V3 (-0.25) 0 0))
             , Position (V4 p' p' p' p')
             , Speed 250
             , SpriteSheet 0 0 32 0 (32 * 5) 32 32 1 5 0
             , UV (V4 (V2 0 1) (V2 (1/5) 1) (V2 (1/5) 0) (V2 0 0))
             )
  newEntity_ ( Size (V4 (V3 (-10) 0 (-10)) (V3 10 0 (-10)) (V3 10 0 10) (V3 (-10) 0 10))
             , Position (V4 p p p p)
             , Speed 250
             , SpriteSheet 0 0 32 0 (32 * 5) 32 32 1 5 0
             , UV (V4 (V2 0 1) (V2 (1/5) 1) (V2 (1/5) 0) (V2 0 0))
             )


  _model
  _view
  _projection $ V2 800 600

  l <- _loadVBOEBO

  _draw l
  liftIO $ threadDelay 20000

  _draw l
  liftIO $ threadDelay 5000000
