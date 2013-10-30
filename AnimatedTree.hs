
module AnimatedTree
   (
     animatedTree
   ) where

import           Control.Lens
import           Control.Applicative ((<$>))
import           Control.Monad.Random

import           Animation
import           Color
import           Tree
import           Vec2
import           PrimGraphics

animatedTree :: RandomGen g => Vec2 -> Color -> Rand g Animation
animatedTree p col = do
   restingTree <- mkTree p 0 0.17 (*0.89) 0.3 25 0

   let trees = map (perturbTree restingTree)
                  (take 20 $ map (\s -> (\i -> s*(fromIntegral i))) [0.002,0.004..])
       graphics = map (Left . mkList . (`proccessTree` col)) trees 

   return $ Animation (\time -> graphics !! ((round $ time / (2/20)) `rem` 20))

perturbTree :: Tree -> (Int -> Float) -> Tree
perturbTree t f = perturb t f 0
   where perturb (Tree (Vec2 x y,r) ts) f i = Tree (Vec2 (x + f i) y, r)
               (map (\t -> perturb t f (i+1)) ts)



