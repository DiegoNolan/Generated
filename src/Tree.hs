
module Tree
   (
     Tree (..)
   , tree
   , proccessTree
   , grassPatch
   , mkTree
   , defTree
   ) where

import           Control.Lens
import           Control.Applicative ((<$>))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Random

import           Color
import           Vec2
import           PrimGraphics

data Tree = Tree (Vec2,Float) [Tree] deriving Show

defTree :: RandomGen g => Vec2 -> Color -> RandT g IO Graphic
defTree start col = tree start 0.17 (*0.89) 0.3 25 col

tree :: RandomGen g =>  Vec2  -> -- Start position
                        Float -> -- start radius
                        (Float -> Float) -> -- how to change radius each step
                        Float -> -- y step per notch
                        Int   -> -- Notches left
                        Color -> 
                        RandT g IO Graphic
tree p r f h n col = do
    t <- mkTree p 0 r f h n 0 
   
    liftIO $ mkList ( proccessTree t col ) >>= mkGraphic

proccessTree :: Tree -> Color -> IO ()
proccessTree (Tree (p,r) []) col = circ p r col
proccessTree (Tree (p,r) ts) col = do
   
   circ p r col

   let prs = map (\(Tree (b,s) _) -> (b,s)) ts

   mapM_ (\(b,s) -> circ b s col) prs

   mapM_ (\(b,s) -> pulley p r b s col) prs

   mapM_ (\t -> proccessTree t col) ts

mkTree :: RandomGen g =>   Vec2   -> -- Start position
                           Float  -> -- previous angle
                           Float  -> -- start radius
                           (Float -> Float) -> -- how to change radius each step
                           Float  -> -- y step per notch
                           Int    -> -- Notches left
                           Int    -> -- Last split
                           RandT g IO Tree
mkTree p _ r _ _ 0 _ = return $ Tree (p,r) []
mkTree p prevA r f h num lstSplit = do

   splitChance <- getRandomR (0,1)

   let n = nodes num lstSplit splitChance
       nSplit = if n == 1 then lstSplit+1 else 0

   angles <- take n <$> getRandomRs (angRange prevA num)

   let nextps = map (\a -> p `add` rotate (Vec2 0 h) a) angles
       psAngs = zip nextps angles

   trees <- mapM (\(np,na) -> mkTree np na (f r) f h (num-1) nSplit) psAngs

   return $ Tree (p,r) trees

angRange :: Float -> -- prev angle
            Int   -> -- segments left
            (Float, Float)
angRange prevAng left
   | left < 5     = curb $ over both (+prevAng) (-pi/5, pi/5) 
   | left < 10    = curb $ over both (+prevAng) (-pi/8, pi/8)
   | otherwise    = curb $ over both (+prevAng) (-pi/10, pi/10)
      where curb (l,r) = ( if l < (-pi/2) then (-pi/2) else l
                         , if r > (pi/2) then (pi/2) else r)

nodes :: Int -> Int -> Float -> Int
nodes left lstSplit p
   | left < 5     = fromLS p 2 3
   | left < 10    = fromLS p 3 5
   | otherwise    = fromLS p 4 7
      where fromLS p l h
               | lstSplit < l = sp 0.9 1 p
               | lstSplit < h = sp 0.7 0.9 p
               | otherwise    = sp 0.5 0.7 p
                  where sp one two p
                           | p < one   = 1
                           | p < two   = 2
                           | otherwise = 3

-- | Grass Stuff --------------------------------------------------------------

-- a blade of crass can just be like a small tree with no branches, so
-- we'll use that

grassPatch :: RandomGen g => Vec2 -> Vec2 -> Color -> RandT g IO Graphic
grassPatch left@(Vec2 xl _) right@(Vec2 xr _) col = do

    let cnt = round $ (xr - xl) * 20

    blades <- patch left right cnt 0.3 0.6

    liftIO $ mkList ( mapM_ (`proccessTree` col) blades) >>= mkGraphic

patch :: RandomGen g =>
         Vec2  -> -- left
         Vec2  -> -- right
         Int   -> -- blades
         Float -> -- min height
         Float -> -- top height
         RandT g IO [Tree]
patch (Vec2 x1 y1) (Vec2 x2 y2) cnt minH maxH = do

   let segs = 5

   segHeights <- (map (\h -> h / fromIntegral segs)) . (take cnt) <$>
         getRandomRs (minH, maxH)

   let xgap = (x2 - x1) / fromIntegral cnt
       ygap = (y2 - y1) / fromIntegral cnt
       xs = [x1,x1+xgap..]
       ys = [y1,y1+ygap..]
       points = take cnt $ zipWith Vec2 xs ys
       pHs = zip points segHeights

   mapM (\(p,h) -> blade p segs 0.022 (*0.8) h 0) pHs

blade :: RandomGen g => 
         Vec2  -> -- starting position
         Int   -> -- segments
         Float -> -- radius
         (Float -> Float) ->
         Float -> -- segment height
         Float -> -- angle
         RandT g IO Tree
blade p 0 r _ _ _ = return $ Tree (p,r) []
blade start cnt r f segH ang = do

   nAng <- getRandomR $ over both (+ang) (-pi/8,pi/8)

   let nPos = start `add` rotate (Vec2 0 segH) nAng

   nextBlade <- blade nPos (cnt-1) (f r) f segH nAng

   return $ Tree (start,r) [nextBlade]











