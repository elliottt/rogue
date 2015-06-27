{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Texture where

import Paths_rogue (getDataDir)

import qualified Control.Exception as X
import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP
import           Data.Typeable (Typeable)
import           Foreign.C.String (peekCString)
import           Foreign.Ptr (nullPtr)
import           Foreign.Storable (Storable(..))
import qualified Graphics.UI.SDL as SDL
import           System.FilePath ((</>))


data SdlError = SdlError String
                deriving (Typeable,Show)

instance X.Exception SdlError

sdlCheck :: Bool -> IO ()
sdlCheck True  = return ()
sdlCheck False = do ptr <- SDL.getError
                    err <- peekCString ptr
                    X.throwIO (SdlError err)

data ImageException = ReadException FilePath String
                      deriving (Typeable,Show)

instance X.Exception ImageException


loadImage :: SDL.Renderer -> FilePath -> IO SDL.Texture
loadImage renderer path =
  do ddir <- getDataDir
     let imagePath = ddir </> path
     e    <- JP.readImage imagePath
     dyn  <- case e of
               Right image -> return image
               Left err    -> X.throwIO (ReadException imagePath err)

     withPixelInfo dyn (\ img @ JP.Image { .. } ->
       do suf <- SDL.createRGBSurface 0
              (fromIntegral imageWidth)
              (fromIntegral imageHeight) 32
              0x000000ff 0x0000ff00 0x00ff0000 0xff000000

          sdlCheck (suf /= nullPtr)

          suf' <- SDL.convertSurfaceFormat suf SDL.SDL_PIXELFORMAT_RGBA8888 0
          sdlCheck (suf' /= nullPtr)

          _ <- SDL.lockSurface suf'
          imageToSurface img =<< peek suf'
          SDL.unlockSurface suf'

          tex <- SDL.createTextureFromSurface renderer suf'
          sdlCheck (tex /= nullPtr)

          SDL.freeSurface suf
          SDL.freeSurface suf'

          return tex)


imageToSurface :: PixelInfo a => JP.Image a -> SDL.Surface -> IO ()
imageToSurface img suf = JP.pixelFoldM write () img
  where
  write () = pokePixel suf


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
