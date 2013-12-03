
module Config
   (
     screenWidth
   , screenHeight
   , aspectRatio
   , scaling
   ) where

screenWidth :: Int
screenWidth = 1280

screenHeight :: Int
screenHeight = 720

aspectRatio :: Floating a => a
aspectRatio = (fromIntegral screenWidth) / (fromIntegral screenHeight)

scaling :: Float -> Float
scaling = (*) 0.1

