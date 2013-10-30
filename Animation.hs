
module Animation
   (
     Animation (..)
   , renderAnimation
   ) where

import PrimGraphics

data Animation = Animation (Double -> DelayedGraphic)

renderAnimation :: Double -> Animation -> IO ()
renderAnimation time (Animation f) = do
   void <- renderDelayed (f time)
   return ()


