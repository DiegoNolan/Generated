
module Tree
   (
     tree
   ) where

import           Control.Applicative ((<$>))
import           Control.Monad.Random

import           Color
import           Vec2
import           PrimGraphics

data Tree = Tree (Vec2,Float) [Tree]

instance Show Tree where
   show (Tree p rest) = show p ++ "\n" ++ (concatMap show rest)

tree :: RandomGen g =>  Vec2  -> -- Start position
                        Float -> -- start radius
                        (Float -> Float) -> -- how to change radius each step
                        Float -> -- y step per notch
                        Int   -> -- Notches left
                        Color -> 
                        Rand g Object
tree p r f h n col = do
   t <- mkTree p 0 r f h n

   return $ Object p (r,r) $ Left $ mkList ( proccessTree t col )

proccessTree :: Tree -> Color -> IO ()
proccessTree (Tree (p,r) []) col = circ p r col
proccessTree (Tree (p,r) ts) col = do
   
   circ p r col

   let prs = map (\(Tree (b,s) _) -> (b,s)) ts

   mapM_ (\(b,s) -> circ b s col) prs

   mapM_ (\(b,s) -> pulley p r b s col) prs

   mapM_ (\t -> proccessTree t col) ts


mkTree :: RandomGen g =>   Vec2  -> -- Start position
                           Float -> -- previous angle
                           Float -> -- start radius
                           (Float -> Float) -> -- how to change radius each step
                           Float -> -- y step per notch
                           Int   -> -- Notches left
                           Rand g Tree
mkTree p _ r _ _ 0 = return $ Tree (p,r) []
mkTree p prevA r f h num = do
   -- detrimen nodes
   splitChance <- getRandom

   let n = nodes splitChance

   angles <- take n <$> getRandomRs (prevA + ( negate $ pi/6 ), prevA + pi/6)

   let nextps = map (\a -> p `add` rotate (0,h) a) angles
       psAngs = zip nextps angles

   trees <- mapM (\(np,na) -> mkTree np na (f r) f h (num-1)) psAngs

   return $ Tree (p,r) trees

nodes :: Float -> Int
nodes f
   | f < 0.7   = 1
   | f < 0.9   = 2
   | otherwise = 3


