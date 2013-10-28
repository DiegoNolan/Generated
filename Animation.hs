
module Animation
   (
     Animation (..)
   ) where

import PrimGraphics

data Animation = Animation (\Double -> DelayedGraphic)


