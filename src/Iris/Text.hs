{-# LANGUAGE CPP #-}

-- | Create text items with FreeType.
--
-- The first draft of this module was built with code and ideas taken from
-- https://github.com/schell/editor

module Iris.Text
       ( TextString (..)
       , loadString
       , loadCharacter
       , Character (..)
       ) where

#if !MIN_VERSION_base(4,8,0)
import           Prelude.Compat ((<$>), (<*>))
#endif

import           Control.Monad
import           Foreign
import           Foreign.C.String
import           Graphics.Rendering.OpenGL hiding (bitmap)
import           Graphics.Rendering.FreeType.Internal
import           Graphics.Rendering.FreeType.Internal.Bitmap
import           Graphics.Rendering.FreeType.Internal.Face
import           Graphics.Rendering.FreeType.Internal.GlyphMetrics hiding (height, width)
import           Graphics.Rendering.FreeType.Internal.GlyphSlot
import           Graphics.Rendering.FreeType.Internal.Library
import           Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import           Graphics.Rendering.FreeType.Internal.Vector
import qualified Linear as L
import           System.IO (hPutStrLn, stderr)


data TextString = TextString
  { textStringString   :: String
  , textStringChars    :: [Character]
  , textStringHeight   :: GLfloat
  , textStringKerning  :: [GLfloat]
  } deriving (Show)


loadString :: FilePath -> String -> Int -> IO TextString
loadString fp s px =
  do ff <- loadFont fp
     faceHeight <- peek $ height ff
     chars <- mapM (loadCharacter ff px) s
     kernPairs' <- kernPairs ff s
     return $ TextString s chars (fromIntegral faceHeight) kernPairs'

-- | Loads the a FreeType font from a file path.
loadFont :: FilePath -> IO FT_Face
loadFont path =
  do -- FreeType (http://freetype.org/freetype2/docs/tutorial/step1.html)
     ft <- freeType

     fontFace ft path


data Character = Character
  { characterChar    :: Char
  , characterTexture :: TextureObject
  , characterSize    :: L.V2 GLsizei
  , characterBearing :: L.V2 GLfloat
  , characterAdvance :: GLfloat
  } deriving (Show)

-- | Loads a character texture using FreeType
loadCharacter :: FT_Face -> Int -> Char -> IO Character
loadCharacter ff px char = do
    runFreeType $ ft_Set_Pixel_Sizes ff 0 (fromIntegral px)

    -- Get the unicode char index.
    chNdx <- ft_Get_Char_Index ff $ fromIntegral $ fromEnum char

    -- Load the glyph into freetype memory.
    runFreeType $ ft_Load_Glyph ff chNdx 0

    -- Get the GlyphSlot.
    slot <- peek $ glyph ff

    runFreeType $ ft_Render_Glyph slot ft_RENDER_MODE_NORMAL

    -- Get the char bitmap.
    bmp <- peek $ bitmap slot
    metrics' <- peek $ metrics slot

    let w'       = fromIntegral $ width bmp
        h'       = fromIntegral $ rows bmp
        bearX    = fromIntegral (horiBearingX metrics') / 64
        bearY    = fromIntegral (horiBearingY metrics') / 64
        advance' = fromIntegral (horiAdvance metrics') / 64

    -- Set the texture params on our bound texture.
    texture Texture2D $= Enabled

    -- Set the alignment to 1 byte.
    rowAlignment Unpack $= 1

    -- Generate an opengl texture.
    tex <- newBoundTexUnit
    printError

    -- Buffer glyph bitmap into texture
    texImage2D
        Texture2D
        NoProxy
        0
        R8
        (TextureSize2D w' h')
        0
        (PixelData Red UnsignedByte $ buffer bmp)
    printError

    textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
    textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
    textureWrapMode Texture2D T $= (Repeated, ClampToEdge)

    return $ Character char tex (L.V2 w' h') (L.V2 bearX bearY) advance'


newBoundTexUnit :: IO TextureObject
newBoundTexUnit = do
    [tex] <- genObjectNames 1
    texture Texture2D $= Enabled
    activeTexture     $= TextureUnit 0
    textureBinding Texture2D $= Just tex
    return tex


-- | Prints any accumulated OpenGL errors.
printError :: IO ()
printError = get errors >>= mapM_ (hPutStrLn stderr . ("GL: "++) . show)



runFreeType :: IO FT_Error -> IO ()
runFreeType m = do
    r <- m
    unless (r == 0) $ fail $ "FreeType Error:" ++ show r


freeType :: IO FT_Library
freeType = alloca $ \p -> do
    runFreeType $ ft_Init_FreeType p
    peek p


fontFace :: FT_Library -> FilePath -> IO FT_Face
fontFace ft fp = withCString fp $ \str ->
    alloca $ \ptr -> do
        runFreeType $ ft_New_Face ft str 0 ptr
        peek ptr

-- | More type-safe rapper around ft_Get_Kerning
getKerning :: FT_Face -> FT_Kerning_Mode -> Char -> Char -> IO FT_Vector
getKerning ff (FT_Kerning_Mode kmint) cl cr  =
  do idxl <- ft_Get_Char_Index ff $ fromIntegral $ fromEnum cl
     idxr <- ft_Get_Char_Index ff $ fromIntegral $ fromEnum cr
     alloca $ \ptr -> do
        runFreeType $ ft_Get_Kerning ff idxl idxr (fromIntegral kmint) ptr
        peek ptr

-- | Gets horizontal kerning as a float
getHorizontalKerning :: FT_Face -> Char -> Char -> IO GLfloat
getHorizontalKerning ff cl cr =
  do (FT_Vector kx _) <- getKerning ff ft_KERNING_DEFAULT cl cr
     return $ fromIntegral kx / 64

-- | Gives the advance delta due to kerning for each character in a string.
kernPairs :: FT_Face -> String -> IO [GLfloat]
kernPairs _ [] = return []
kernPairs ff s = (++) <$> mapM (uncurry $ getHorizontalKerning ff) pairs <*> return [0]
  where pairs = zip s (tail s)
