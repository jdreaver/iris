-- | Create text items with FreeType.
--
-- The first draft of this module was built with code and ideas taken from
-- https://github.com/schell/editor

module Iris.Text
       ( loadCharacter
       ) where


import           Control.Monad
import           Graphics.Rendering.OpenGL hiding (bitmap)
import           Graphics.Rendering.FreeType.Internal
import           Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import           Graphics.Rendering.FreeType.Internal.Library
import           Graphics.Rendering.FreeType.Internal.FaceType
import           Graphics.Rendering.FreeType.Internal.Face
import           Graphics.Rendering.FreeType.Internal.GlyphSlot
import           Foreign
import           Foreign.C.String
import           Graphics.Rendering.FreeType.Internal.Bitmap
import           System.IO (hPutStrLn, stderr)

-- | Loads the a FreeType font from a file path.
loadFont :: FilePath -> IO FT_Face
loadFont path =
  do -- FreeType (http://freetype.org/freetype2/docs/tutorial/step1.html)
     ft <- freeType

     fontFace ft path

-- | Returns a TextureObject for a given character.
loadCharacter :: FilePath -> Char -> Int -> Int -> IO TextureObject
loadCharacter path char px texUnit = do
    ff <- loadFont path
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

    let w' = fromIntegral $ width bmp
        h' = fromIntegral $ rows bmp

    -- Set the texture params on our bound texture.
    texture Texture2D $= Enabled

    -- Set the alignment to 1 byte.
    rowAlignment Unpack $= 1

    -- Generate an opengl texture.
    tex <- newBoundTexUnit texUnit
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

    return tex


newBoundTexUnit :: Int -> IO TextureObject
newBoundTexUnit u = do
    [tex] <- genObjectNames 1
    texture Texture2D $= Enabled
    activeTexture     $= TextureUnit (fromIntegral u)
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
