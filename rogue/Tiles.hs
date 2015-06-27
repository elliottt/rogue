{-# LANGUAGE RecordWildCards #-}

module Tiles where

import Paths_rogue (getDataDir)
import Texture
import TextureAtlas

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import           Foreign.C.Types (CInt)
import           Foreign.Marshal.Alloc (calloc)
import           Foreign.Marshal.Utils (with)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (poke)
import qualified Graphics.UI.SDL as SDL
import           System.FilePath ((</>))


data Tile = Tile { tileTexture :: !SDL.Texture
                 , tileRect    :: !(Ptr SDL.Rect)
                 , tileWidth   :: !CInt
                 , tileHeight  :: !CInt
                 }

loadTiles :: SDL.Renderer -> IO (Map.Map String Tile)
loadTiles renderer =
  do ddir  <- getDataDir
     bytes <- L.readFile (ddir </> "sprites" </> "buildingTiles_sheet.xml")
     let Just TextureAtlas { .. } = parseTextureAtlas bytes

     png   <- loadImage renderer ("sprites" </> taImagePath)

     mapM (mkTile png) taSubTextures

mkTile :: SDL.Texture -> SubTexture -> IO Tile
mkTile tileTexture SubTexture { .. } =
  do tileRect <- calloc
     poke tileRect SDL.Rect { SDL.rectX = fromIntegral stX
                            , SDL.rectY = fromIntegral stY
                            , SDL.rectW = fromIntegral stWidth
                            , SDL.rectH = fromIntegral stHeight }
     return Tile { tileWidth  = fromIntegral stWidth
                 , tileHeight = fromIntegral stHeight
                 , .. }

drawTile :: SDL.Renderer -> Tile -> Int -> Int -> Int -> Int -> IO ()
drawTile renderer Tile { .. } x y w h =
  do _ <- with dstRect (SDL.renderCopy renderer tileTexture tileRect)
     return ()
  where
  dstRect = SDL.Rect { SDL.rectX = fromIntegral x
                     , SDL.rectY = fromIntegral y
                     , SDL.rectW = fromIntegral w
                     , SDL.rectH = fromIntegral h }

