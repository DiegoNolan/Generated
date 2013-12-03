
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

equallySpaced :: Float -> Float -> Int -> [Float]
equallySpaced l r n = [l,l+gap..r]
   where gap = (r - l) / (fromIntegral (n-1))

animatedTree :: RandomGen g => g -> Vec2 -> Color -> IO Animation
animatedTree gen p col = do
   let (restingTree,ng) = runRand (mkTree p 0 0.17 (*0.89) 0.3 25 0) gen
       (tOffset, _) = randomR (0,10) gen

   let trees = map (rotatePerturb restingTree) (equallySpaced (-pi/150) (pi/150) 60)
       graphics = map (Left . mkList . (`proccessTree` col)) trees

   newGraphics <- mapM renderDelayed graphics
   let doubledGraphics = newGraphics ++ reverse newGraphics

   return $ Animation (\time -> doubledGraphics !! ((round $ (time + tOffset) /
               (8/120)) `rem` 120))

rotatePerturb :: Tree -> Float -> Tree
rotatePerturb t@(Tree (p,_) _) scale = rotPer t p scale
   where rotPer (Tree (v,r) ts) p s = Tree (nv,r) (map (\t -> rotPer t p s) ts)
            where nv = rr `add` p
                  vr = v `sub` p
                  rr = rotate vr s

