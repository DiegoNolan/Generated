{-# LANGUAGE TemplateHaskell #-}

module Tree
   (
     TreeSettings (..)
   , tree
   , defTree
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


