{-# LANGUAGE CPP #-}

-- | Visual to render lines of text.

module Iris.Visuals.Text
       ( TextSpec (..)
       , textSpec
       , TextItem (..)
       , textInit
       , makeText
       ) where

#if !MIN_VERSION_base(4,8,0)
import           Prelude.Compat ((<$>), (<*>))
#endif

import qualified Data.Vector.Storable as V
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L

import           Iris.SceneGraph
import           Iris.Text
import           Iris.Visuals.Image

data TextItem = TextItem [ImageItem]

data TextSpec = TextSpec
  { textSpecString   :: String
  , textFontPath     :: FilePath
  , textSpecPos      :: L.V2 GL.GLfloat
  , textSpecHeight   :: GL.GLfloat
  , textSpecFontSize :: Int
  } deriving (Show)

textSpec :: TextSpec
textSpec = TextSpec "" "" (L.V2 0 0) 1 512


textInit :: TextSpec -> IO DrawNode
textInit spec =
  do item <- makeText spec
     return $ DrawNode (drawText item)


-- | Create a `TextItem` by creating textures for individual characters, and
-- creating a quadrilateral
makeText :: TextSpec -> IO TextItem
makeText (TextSpec s fp pos h px) =
  do textString <- loadString fp s px
     images <- makeText' (textStringChars textString) pos h px
     return $ TextItem images

makeText' :: [Character] -> L.V2 GL.GLfloat -> GL.GLfloat -> Int -> IO [ImageItem]
makeText' [] _ _ _               = return []
makeText' (c:s) (L.V2 x y) th px =
  do
     let (Character _ to (L.V2 cw ch) (L.V2 bx by) adv) = c
         scale = th / fromIntegral px
         xpos  = x + bx * scale
         ypos  = y - (fromIntegral ch - by) * scale
         w     = fromIntegral cw * scale
         h     = fromIntegral ch * scale
         verts = V.fromList [ L.V3 xpos       ypos       0
                            , L.V3 (xpos + w) ypos       0
                            , L.V3 (xpos + w) (ypos + h) 0
                            , L.V3 xpos       (ypos + h) 0
                            ]
         x'    = x + adv * scale
     (:) <$> makeImage (ImageSpec to verts) <*> makeText' s (L.V2 x' y) th px


-- | Draw a given line item to the current OpenGL context
drawText :: TextItem -> DrawFunc
drawText (TextItem images) dd = mapM_ (`drawImage` dd) images
