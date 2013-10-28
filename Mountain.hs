
module Mountain
   (
     mountain
   ) where

import           Control.Monad.Random
import           System.Random

import qualified Graphics.Rendering.OpenGL as GL

import           Color
import           PrimGraphics
import           Vec2

weave :: [a] -> [a] -> [a]
weave (x:xs) (y:ys) = x : y : weave xs ys
weave xs [] = xs
weave [] ys = ys

mountain :: RandomGen g => Vec2 -> Vec2 -> Float -> Color -> Rand g DelayedGraphic
mountain tl@(Vec2 x1 y1) tr@(Vec2 x2 y2) bottom color = do
   
   let left = min x1 y2
       right = max x1 x2
       dist = right - left
   
   ys <- mpd y1 y2 2 (/2) 10

   -- can actually calculate this length and probably should for efficiency
   let l = length ys
       gap = dist / ( fromIntegral $ l - 1 )
       xs = take l $ [left,(left+gap)..]
       topPoints = zipWith Vec2 xs ys ++ [(Vec2 right bottom),(Vec2 left bottom)]
       botPoints = map (`Vec2` bottom) xs
       points = weave topPoints botPoints

   return $ Left $ mkList $ do
            GL.color color
            triangleStrip points

mpd :: RandomGen g => Float   -> -- left
                      Float   -> -- right
                      Float   -> -- half rang
                      (Float -> Float) -> -- range change on recursion
                      Int     -> -- how deep to go
                      Rand g [Float]
mpd l r h f d = do
   mid <- midPointDisp l r h f d
   return $ [l] ++ mid ++ [r]

midPointDisp :: RandomGen g => Float   -> -- left
                               Float   -> -- right
                               Float   -> -- half rang
                               (Float -> Float) -> -- range change on recursion
                               Int     -> -- how deep to go
                               Rand g [Float]
midPointDisp _ _ _ _ 0 = return []
midPointDisp l r h f d = do
   let mid = (l + r) / 2
   m <- getRandomR (mid + h,mid - h)
   left <- midPointDisp l m (f h) f (d-1)
   right <- midPointDisp m r (f h) f (d-1)
   return $ left ++ [m] ++ right

