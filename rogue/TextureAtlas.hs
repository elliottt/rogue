{-# LANGUAGE RecordWildCards #-}

module TextureAtlas (
  TextureAtlas(..),
  SubTexture(..),
  parseTextureAtlas
  ) where

import           Control.Monad (guard)
import qualified Data.ByteString.Lazy as L
import           Data.List (find)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Text.XML.Light


-- | A record of sub-textures in a sprite-sheet.
data TextureAtlas       = TextureAtlas { taImagePath   :: FilePath
                                       , taSubTextures :: Map.Map String SubTexture
                                       } deriving (Show)

data SubTexture         = SubTexture   { stName        :: String
                                       , stX           :: !Int
                                       , stY           :: !Int
                                       , stWidth       :: !Int
                                       , stHeight      :: !Int
                                       } deriving (Show)

-- | Parse a description of the texture sheet.
parseTextureAtlas :: L.ByteString -> Maybe TextureAtlas
parseTextureAtlas source = textureAtlas (parseXML source)

textureAtlas :: [Content] -> Maybe TextureAtlas
textureAtlas cs =
  do (attrs,children) <- el "TextureAtlas" cs
     taImagePath      <- attr "imagePath" attrs
     let taSubTextures =
           Map.fromList [ (stName st, st) | st <- mapMaybe subTexture children ]
     return TextureAtlas { .. }

subTexture :: Content -> Maybe SubTexture
subTexture (Elem Element { .. }) =
  do guard (qName elName == "SubTexture")
     stName <- attr "name"   elAttribs
     x      <- attr "x"      elAttribs
     y      <- attr "y"      elAttribs
     width  <- attr "width"  elAttribs
     height <- attr "height" elAttribs
     return SubTexture { stX      = read x
                       , stY      = read y
                       , stWidth  = read width
                       , stHeight = read height
                       , .. }
subTexture _ = Nothing

el :: String -> [Content] -> Maybe ([Attr],[Content])
el key cs =
  do Elem Element { .. } <- find isElem cs
     return (elAttribs, elContent)
  where
  isElem (Elem Element { .. }) = qName elName == key
  isElem _                     = False

attr :: String -> [Attr] -> Maybe String
attr key as =
  do Attr { .. } <- find isKey as
     return attrVal
  where
  isKey Attr { .. } = qName attrKey == key
