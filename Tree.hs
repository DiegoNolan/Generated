{-# LANGUAGE TemplateHaskell #-}

module Tree
   (
     TreeSettings (..)
   , tree
   , grassPatch
   , defTree
   , defLeaf
   ) where

import           Control.Lens
import           Control.Applicative ((<$>))
import           Control.Monad.Random

import           Color
import           Vec2
import           PrimGraphics

data Tree = Tree (Vec2,Float) [Tree]

data TreeSettings = TreeSettings
      {                 -- gen    lastsplit
        _chanceToSplit  :: Int -> Int -> Float -> Int
                        -- generation, previous Angle
      , _angleRange     :: Int -> Float -> (Float, Float)
      , _segHeight      :: Float
      , _totalGens      :: Int
      , _radiusChange   :: Float -> Float
      }

makeLenses ''TreeSettings

defaultTree :: TreeSettings
defaultTree = TreeSettings
                  chToSp
                  angRng
                  0.7
                  2
                  (*0.8)
   where chToSp g l p
            | g < 3        = if l == 0 then chance 1 1 p else chance 0.9 1 p 
            | g < 5        = if l <= 3 then chance 0.7 0.9 p else chance 0.5 0.7 p
            | otherwise    = if l <= 3 then chance 0.5 0.7 p else chance 0.4 0.6 p
         chance one two p
            | p < one   = 1
            | p < two   = 2
            | otherwise = 3

         angRng g pA
            | g < 10    = (pi/5,pi/5)
            | otherwise = (pi/6,pi/6)
               

instance Show Tree where
   show (Tree p rest) = show p ++ "\n" ++ (concatMap show rest)

defTree :: RandomGen g => Vec2 -> Color -> Rand g Object
defTree start col = tree start 0.17 (*0.89) 0.3 25 col

tree :: RandomGen g =>  Vec2  -> -- Start position
                        Float -> -- start radius
                        (Float -> Float) -> -- how to change radius each step
                        Float -> -- y step per notch
                        Int   -> -- Notches left
                        Color -> 
                        Rand g Object
tree p r f h n col = do
   t <- mkTree p 0 r f h n 0
   -- t <- mkTreeSet defaultTree p 0.1 0 0 0

   return $ Object p (r,r) $ Left $ mkList ( proccessTree t col )

proccessTree :: Tree -> Color -> IO ()
proccessTree (Tree (p,r) []) col = circ p r col
proccessTree (Tree (p,r) ts) col = do
   
   circ p r col

   let prs = map (\(Tree (b,s) _) -> (b,s)) ts

   mapM_ (\(b,s) -> circ b s col) prs

   mapM_ (\(b,s) -> pulley p r b s col) prs

   mapM_ (\t -> proccessTree t col) ts

mkTreeSet ::   RandomGen g    =>
               TreeSettings   -> -- tree settings
               Vec2           -> -- position
               Float          -> -- radius
               Float          -> -- angle
               Int            -> -- generation
               Int            -> -- last split
               Rand g Tree
mkTreeSet settings p ang r gen last = do

   if gen >= settings^.totalGens
   then return $ Tree (p,r) []
   else do

      splitChance <- getRandom

      let n = (settings^.chanceToSplit) gen last splitChance
          lsp = if n == 1 then last+1 else 0

      angles <- take n <$> getRandomRs ((settings^.angleRange) gen ang)

      let nextps = map (\a -> p `add` rotate (0,settings^.segHeight) a) angles
          psAngs = zip nextps angles
          nRad = (settings^.radiusChange) r

      trees <- mapM (\(np,na) -> mkTreeSet settings np nRad na (gen+1) lsp) psAngs

      return $ Tree (p,r) trees

mkTree :: RandomGen g =>   Vec2   -> -- Start position
                           Float  -> -- previous angle
                           Float  -> -- start radius
                           (Float -> Float) -> -- how to change radius each step
                           Float  -> -- y step per notch
                           Int    -> -- Notches left
                           Int    -> -- Last split
                           Rand g Tree
mkTree p _ r _ _ 0 _ = return $ Tree (p,r) []
mkTree p prevA r f h num lstSplit = do

   splitChance <- getRandomR (0,1)

   let n = nodes num lstSplit splitChance
       nSplit = if n == 1 then lstSplit+1 else 0

   angles <- take n <$> getRandomRs (angRange prevA num)

   let nextps = map (\a -> p `add` rotate (0,h) a) angles
       psAngs = zip nextps angles

   trees <- mapM (\(np,na) -> mkTree np na (f r) f h (num-1) nSplit) psAngs

   return $ Tree (p,r) trees

angRange ::  Float -> -- prev angle
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

-- | Leaf stuff ---------------------------------------------------------------

-- leafs can just be goofy trees

defLeaf ::  RandomGen g =>
            Vec2        -> -- Start position
            Float       -> -- angle
            Color       -> -- color
            Rand g Object
defLeaf position angle color = do

   let radFunc 1 = 0.01
       radFunc 2 = 0.04
       radFunc 3 = 0.06
       radFunc 4 = 0.07
       radFunc 5 = 0.07
       radFunc 6 = 0.06
       radFunc 7 = 0.04
       radFunc _ = 0.00001

   leaf <- cherryLeaf position 7 0.001 radFunc 0.15 angle

   return $ Object position (0,0) $ Left $ mkList (proccessTree leaf color)

cherryLeaf ::  RandomGen g =>
               Vec2        -> -- start position
               Int         -> -- segments left
               Float       -> -- radius
               (Int -> Float) -> -- function to change the radius, gen -> new rad
               Float       -> -- segment height
               Float       -> -- angle
               Rand g Tree
cherryLeaf start 0 r _ _ _ = return $ Tree (start,r) []
cherryLeaf start cnt r f segH ang = do

   nAng <- getRandomR $ over both (+ang) (-pi/8,pi/8)
   
   let nPos = start `add` rotate (0,segH) nAng

   nextPart <-  cherryLeaf nPos (cnt-1) (f cnt) f segH nAng

   return $ Tree (start,r) [nextPart]

-- | Grass Stuff --------------------------------------------------------------

-- a blade of crass can just be like a small tree with no branches, so
-- we'll use that

grassPatch :: RandomGen g => Vec2 -> Vec2 -> Color -> Rand g Object
grassPatch left right col = do

   let cnt = round $ ((right^._1) - (left^._1)) * 20

   blades <- patch left right cnt 0.3 0.6
   
   return $ Object (0,0) (0,0) $
      Left $ mkList ( mapM_ (`proccessTree` col) blades)

patch :: RandomGen g =>
         Vec2  -> -- left
         Vec2  -> -- right
         Int   -> -- blades
         Float -> -- min height
         Float -> -- top height
         Rand g [Tree]
patch (x1,y1) (x2,y2) cnt minH maxH = do

   let segs = 5

   segHeights <- (map (\h -> h / fromIntegral segs)) . (take cnt) <$>
         getRandomRs (minH, maxH)

   let xgap = (x2 - x1) / fromIntegral cnt
       ygap = (y2 - y1) / fromIntegral cnt
       xs = [x1,x1+xgap..]
       ys = [y1,y1+ygap..]
       points = take cnt $ zip xs ys
       pHs = zip points segHeights

   mapM (\(p,h) -> blade p segs 0.022 (*0.8) h 0) pHs

blade :: RandomGen g => 
         Vec2  -> -- starting position
         Int   -> -- segments
         Float -> -- radius
         (Float -> Float) ->
         Float -> -- segment height
         Float -> -- angle
         Rand g Tree
blade p 0 r _ _ _ = return $ Tree (p,r) []
blade start cnt r f segH ang = do

   nAng <- getRandomR $ over both (+ang) (-pi/8,pi/8)

   let nPos = start `add` rotate (0,segH) nAng

   nextBlade <- blade nPos (cnt-1) (f r) f segH nAng

   return $ Tree (start,r) [nextBlade]











