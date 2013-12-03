
module AnimatedTree
   (
     animatedTree
   ) where

import           Control.Lens
import           Control.Applicative ((<$>))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Random

import           Animation
import           Color
import           Tree
import           Vec2
import           PrimGraphics

equallySpaced :: Float -> Float -> Int -> [Float]
equallySpaced l r n = [l,l+gap..r]
   where gap = (r - l) / (fromIntegral (n-1))

animatedTree :: RandomGen g => Vec2 -> Color -> RandT g IO Animation
animatedTree p col = do
    restingTree <- mkTree p 0 0.17 (*0.89) 0.3 25 0
    tOffset <- getRandomR (0,10)

    let trees = map (rotatePerturb restingTree)
            (equallySpaced (-pi/150) (pi/150) 60)
    graphics <- liftIO $
        mapM (\t ->  mkList (proccessTree t col) >>= mkGraphic) trees

    let doubledGraphics = graphics ++ reverse graphics

    return $ (\time -> doubledGraphics !! ((round $ (time + tOffset) /
               (8/120)) `rem` 120))

rotatePerturb :: Tree -> Float -> Tree
rotatePerturb t@(Tree (p,_) _) scale = rotPer t p scale
   where rotPer (Tree (v,r) ts) p s = Tree (nv,r) (map (\t -> rotPer t p s) ts)
            where nv = rr `add` p
                  vr = v `sub` p
                  rr = rotate vr s

