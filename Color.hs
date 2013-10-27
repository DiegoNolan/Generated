
module Color where

import Data.Bits
import Data.Word

import qualified Graphics.Rendering.OpenGL as GL

type Color = GL.Color4 GL.GLfloat

gray :: GL.GLfloat -> Color
gray b = GL.Color4 b b b 1

white :: Color
white = GL.Color4 1 1 1 1

black :: Color
black = GL.Color4 0 0 0 1

red :: Color
red = GL.Color4 1 0 0 1

green :: Color
green = GL.Color4 0 1 0 1

blue :: Color
blue = GL.Color4 0 0 1 1

yellow :: Color
yellow = GL.Color4 1 1 0 1

cyan :: Color
cyan = GL.Color4 0 1 1 1

magenta :: Color
magenta = GL.Color4 1 0 1 1

-- |---------------------------------------------------------------------------
-- | 8 Bit Colors -------------------------------------------------------------
-- |---------------------------------------------------------------------------

eightBitColors :: [Color]
eightBitColors = map toRGB word8s

word8s :: [Word8]
word8s = enumFrom minBound

toRGB :: Word8 -> Color
toRGB w = GL.Color4 (toRed w) (toGreen w) (toBlue w) 1

toBlue :: Word8 -> GL.GLfloat
toBlue w = (fromIntegral (w .&. 3)) * (1/3)

toGreen :: Word8 -> GL.GLfloat
toGreen w = (fromIntegral ((shift w (-2) .&. 7))) * (1/7)

toRed :: Word8 -> GL.GLfloat
toRed w = (fromIntegral ((shift w (-5) .&. 7))) * (1/7)

