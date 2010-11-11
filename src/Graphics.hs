{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}

module Graphics (
    -- * OpenGL/SDL
    initGraphics
  , updateScreen
  , clearScreen

    -- * Matrix Manipulation
  , translate
  , rotate
  , scale
  , withMatrix

    -- * Rendering
  , vertex2d
  , color3, withColor3_
  , color4, withColor4_
  , setLineWidth
  , getLineWidth
  , setPointSize
  , getPointSize
  , Render(..)

    -- * Texturing
  , Texture
  , setTexture2d
  , clearTexture2d
  , texCoord2d
  , newTexture
  , surfaceToTexture
  , loadTexture

    -- * Re-exported
  , GL.renderPrimitive
  , GL.PrimitiveMode(..)
  , SDL.quit
  ) where

import Math.AffinePlane
import Math.Utils

import Foreign.C.Types (CInt)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr)
import Graphics.Rendering.OpenGL.GL (GLuint,HasSetter(..))
import qualified Graphics.Rendering.OpenGL.GL  as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified Graphics.UI.SDL               as SDL
import qualified Graphics.UI.SDL.Image         as SDL


-- OpenGL/SDL ------------------------------------------------------------------

initGraphics :: String -> Int -> Int -> IO ()
initGraphics t w h = do

  -- SDL
  SDL.setCaption t ""
  _ <- SDL.setVideoMode w h 24 [SDL.OpenGL]

  -- OpenGL
  GL.clearColor           $= GL.Color4 0 0 0 0
  GL.clearDepth           $= 1
  GL.depthFunc            $= Just GL.Less
  GL.shadeModel           $= GL.Smooth
  GL.matrixMode           $= GL.Projection
  GL.loadIdentity
  GLU.perspective 45 (fromIntegral w / fromIntegral h) 0.1 100
  GL.matrixMode           $= GL.Modelview 0

  GL.texture GL.Texture2D $= GL.Enabled
  GL.blend                $= GL.Enabled
  GL.blendFunc            $= (GL.SrcAlpha,GL.OneMinusSrcAlpha)

  GL.flush


-- | Swap the GL buffers.
updateScreen :: IO ()
updateScreen  = do
  GL.flush
  SDL.glSwapBuffers
  SDL.delay 1

-- | Clear the scene.
clearScreen :: IO ()
clearScreen  = do
  GL.clear [GL.DepthBuffer, GL.ColorBuffer]
  GL.loadIdentity


-- Matrix Manipulation ---------------------------------------------------------

translate :: GLfloat -> GLfloat -> GLfloat -> IO ()
translate x y z = GL.translate (GL.Vector3 x y z)

rotate :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
rotate r x y z = GL.rotate r (GL.Vector3 x y z)

scale :: GLfloat -> GLfloat -> GLfloat -> IO ()
scale  = GL.scale

withMatrix :: IO a -> IO a
withMatrix = GL.unsafePreservingMatrix


-- Rendering -------------------------------------------------------------------

vertex2d :: GLfloat -> GLfloat -> IO ()
vertex2d x y = GL.vertex (GL.Vertex2 x y)

color3 :: GLfloat -> GLfloat -> GLfloat -> IO ()
color3 r g b = color4 r g b 1.0

color4 :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
color4 r g b a = GL.color (GL.Color4 r g b a)

withColor3_ :: GLfloat -> GLfloat -> GLfloat -> IO () -> IO ()
withColor3_ r g b = withColor4_ r g b 1.0

withColor4_ :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO () -> IO ()
withColor4_ r g b a action = do
  GL.Color4 r0 g0 b0 a0 <- GL.get GL.currentColor
  color4 r g b a
  action
  color4 r0 g0 b0 a0

setLineWidth :: GLfloat -> IO ()
setLineWidth w = GL.lineWidth $= w

getLineWidth :: IO GLfloat
getLineWidth  = GL.get GL.lineWidth

setPointSize :: GLfloat -> IO ()
setPointSize s = GL.pointSize $= s

getPointSize :: IO GLfloat
getPointSize  = GL.get GL.pointSize

-- | Things that can be rendered.
class Render a where
  render :: a -> IO ()

instance Render a => Render (Maybe a) where
  render Nothing  = return ()
  render (Just a) = render a

instance Render a => Render [a] where
  render = mapM_ render

instance Render (Point GLfloat) where
  render (Point x y) = vertex2d x y

instance Render (Line GLfloat) where
  render (Line a b) = render a >> render b


-- Texturing -------------------------------------------------------------------

type Texture = GL.TextureObject

setTexture2d :: Texture -> IO ()
setTexture2d tex = GL.textureBinding GL.Texture2D $= Just tex

clearTexture2d :: IO ()
clearTexture2d  = GL.textureBinding GL.Texture2D $= Nothing

texCoord2d :: GLfloat -> GLfloat -> IO ()
texCoord2d x y = GL.texCoord (GL.TexCoord2 x y)

newTexture :: IO Texture
newTexture  = do
  ns <- GL.genObjectNames 1
  case ns of
    [tex] -> return tex
    _     -> fail "Graphics.newTexture: Couldn't generate a TextureObject"

surfaceToTexture :: SDL.Surface -> IO Texture
surfaceToTexture suf = do
  tex@(GL.TextureObject i) <- newTexture
  withForeignPtr suf $ \ sufP -> do
    res <- c_SDL_Surface_to_glTextureObject sufP i
    case res of
      0 -> return tex
      1 -> fail "Graphics.surfaceToTexture: Invalid surface"
      _ -> fail "Graphics.surfaceToTexture: Unexpected error"

loadTexture :: FilePath -> IO Texture
loadTexture path = surfaceToTexture =<< SDL.load path

foreign import ccall unsafe "sdl-opengl.h SDL_Surface_to_glTextureObject"
  c_SDL_Surface_to_glTextureObject :: Ptr SDL.SurfaceStruct -> GLuint -> IO CInt
