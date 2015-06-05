{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Paths_rogue (getDataDir)
import TextureAtlas

import Rogue.State

import           Control.Concurrent (forkIO,threadDelay)
import           Control.Concurrent.STM (TVar,newTVar,readTVar,writeTVar)
import qualified Control.Exception as X
import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP
import qualified Data.ByteString.Lazy as L
import           Data.Bits (shiftL,(.|.))
import           Data.Word (Word32)
import           Foreign.C.String (withCString)
import           Foreign.C.Types (CInt)
import           Foreign.Ptr (Ptr,nullPtr,plusPtr)
import           Foreign.Storable (Storable(..))
import qualified Graphics.UI.SDL as SDL
import           System.FilePath ((</>))


main :: IO ()
-- main  =
--   do ddir  <- getDataDir
--      bytes <- L.readFile (ddir </> "sprites" </> "buildingTiles_sheet.xml")
--      print (parseTextureAtlas bytes)


main = withSDL $
  do window   <- createWindow
     renderer <- SDL.createRenderer window (-1) 0
     SDL.setRenderDrawColor renderer 255 0 0 255
     png <- loadImage renderer ("sprites" </> "buildingTiles_sheet.png")
     SDL.renderClear renderer
     SDL.renderCopy renderer png nullPtr nullPtr
     SDL.renderPresent renderer
     threadDelay 2000000
     SDL.destroyWindow window

withSDL :: IO () -> IO ()
withSDL  = X.bracket_ (SDL.init SDL.SDL_INIT_VIDEO) SDL.quit

loadImage :: SDL.Renderer -> FilePath -> IO SDL.Texture
loadImage renderer path =
  do ddir <- getDataDir
     e    <- JP.readImage (ddir </> path)
     dyn  <- case e of
               Right image -> return image
               Left err    -> fail err

     withPixelInfo dyn (\ img @ JP.Image { .. } ->
       do suf <- SDL.createRGBSurface 0
              (fromIntegral imageWidth)
              (fromIntegral imageHeight) 32
              0x000000ff 0x0000ff00 0x00ff0000 0xff000000

          print (suf == nullPtr)

          suf' <- SDL.convertSurfaceFormat suf SDL.SDL_PIXELFORMAT_RGBA8888 0
          print (suf' == nullPtr)

          SDL.lockSurface suf'
          imageToSurface img =<< peek suf'
          SDL.unlockSurface suf'

          tex <- SDL.createTextureFromSurface renderer suf'
          print (tex == nullPtr)

          SDL.freeSurface suf
          SDL.freeSurface suf'

          return tex)


imageToSurface :: PixelInfo a => JP.Image a -> SDL.Surface -> IO ()
imageToSurface img suf = JP.pixelFoldM write () img
  where
  write () = pokePixel suf


createWindow :: IO SDL.Window
createWindow  =
  withCString "Rogue!" $ \ str ->
  do SDL.createWindow str SDL.SDL_WINDOWPOS_UNDEFINED
                          SDL.SDL_WINDOWPOS_UNDEFINED
                          640 480 0

withPixelInfo :: JP.DynamicImage
              -> (forall a. PixelInfo a => JP.Image a -> r)
              -> r
withPixelInfo dyn k =
  case dyn of
    JP.ImageY8     _img -> error "ImageY8"
    JP.ImageY16    _img -> error "ImageY16"
    JP.ImageYF     _img -> error "ImageYF"
    JP.ImageYA8    _img -> error "ImageYA8"
    JP.ImageYA16   _img -> error "ImageYA16"
    JP.ImageRGB8   _img -> error "ImageRGB8"
    JP.ImageRGB16  _img -> error "ImageRGB16"
    JP.ImageRGBF   _img -> error "ImageRGBF"
    JP.ImageRGBA8  img -> k img
    JP.ImageRGBA16 _img -> error "ImageRGBA16"
    JP.ImageYCbCr8 _img -> error "ImageYCbCr8"
    JP.ImageCMYK8  _img -> error "ImageCMYK8"
    JP.ImageCMYK16 _img -> error "ImageCMYK16"

class (JP.Pixel a) => PixelInfo a where
  pokePixel  :: SDL.Surface -> Int -> Int -> a -> IO ()

instance PixelInfo JP.PixelRGBA8 where
  pokePixel SDL.Surface { .. } x y (JP.PixelRGBA8 r g b a) =
    do w <- SDL.mapRGBA surfaceFormat r g b a
       let off = sizeOf w * (fromIntegral surfaceW * y + x)
       pokeByteOff surfacePixels off w
