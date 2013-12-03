
module Animation
   (
     Animation (..)
   --, renderAnimation
   ) where

import PrimGraphics

-- data Animation = Animation (Double -> DelayedGraphic)
type Animation = Double -> Graphic

{-
renderAnimation :: Double -> Animation -> IO ()
renderAnimation time (Animation f) = do
   void <- renderDelayed (f time)
   return ()
-}


