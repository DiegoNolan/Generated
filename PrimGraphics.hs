
module PrimGraphics
   (
     DelayedGraphic
   , Object (..)
   , renderDelayed
   , renderObject
   , mkList
   , convexShape
   , circle
   , square
   , circ
   , pulley
   , triangleStrip
   ) where

import           Control.Lens
import           Control.Monad (forM_)

import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

import           Color
import           Config
import           Vec2

type DelayedGraphic = Either (IO GL.DisplayList) GL.DisplayList

data AABB = AABB
   {
     left   :: !Float
   , right  :: !Float
   , top    :: !Float
   , bottom :: !Float
   }

-- Objects are positioned at their bottom center
data Object = Object
   {
     pos    :: Vec2
   , size   :: Vec2
   , gra    :: DelayedGraphic
   }

circle_sides :: Int
circle_sides = 50

renderDelayed :: DelayedGraphic -> IO DelayedGraphic
renderDelayed (Left f) = f >>= (renderDelayed . Right)
renderDelayed dg@(Right d) = GL.callList d >> return dg

renderObject :: Object -> IO Object
renderObject (Object p z g) = renderDelayed g >>= return . Object p z


toSX :: Float -> GL.GLfloat
toSX = realToFrac . scaling

toSY :: Float -> GL.GLfloat
toSY y = aspectRatio * ( realToFrac $ scaling y)

vertex :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> IO ()
vertex x y z = GL.vertex $ GL.Vertex3 x y z

circle :: Vec2 -> Float -> Color -> Object
circle cent r c = Object cent (r*2,r*2) (Left (mkList $ circ cent r c))

mkList :: IO () -> IO GL.DisplayList
mkList f = GL.defineNewList GL.Compile f

square :: Vec2 -> Vec2 -> Color -> IO ()
square tl@(x1,y1) br@(x2,y2) col = do
   GL.color col
   convexShape [tl,(x2,y1),br,(x1,y2)] 

circ :: Vec2 -> Float -> Color -> IO ()
circ (xrel,yrel) r c = do
      GL.color c
      GL.renderPrimitive GL.TriangleFan $ do
         vertex x y 0
         vertex ((toSX r) + x) (y) 0
         forM_ [0,theta..(2*pi)] $ \a -> do
            vertex ( (toSX $ r * cos a) + x) ((toSY $ r * sin a) + y) 0
   where x = toSX xrel
         y = toSY yrel
         theta = (2*pi) / ( fromIntegral circle_sides )

convexShape :: [Vec2] -> IO ()
convexShape (x:y:z:rs) = GL.renderPrimitive GL.Triangles $ conv x y (z:rs)
   where conv _ _ [] = return ()
         conv f@(x1,y1) l@(x2,y2) ((r@(x3,y3)):rest) = do
            vertex (toSX x1) (toSY y1) 0
            vertex (toSX x2) (toSY y2) 0
            vertex (toSX x3) (toSY y3) 0
            conv f r rest
convexShape _  = error "Too few points"

triangleStrip :: [Vec2] -> IO ()
triangleStrip xs = GL.renderPrimitive GL.Triangles $ triStrip xs
   where triStrip ((x1,y1):(x2,y2):(x3,y3):rest) = do
            vertex (toSX x1) (toSY y1) 0
            vertex (toSX x2) (toSY y2) 0
            vertex (toSX x3) (toSY y3) 0
            triStrip ((x2,y2):(x3,y3):rest)
         triStrip _ = return ()

pulley :: Vec2 -> Float -> Vec2 -> Float -> Color -> IO ()
pulley c1 r1 c2 r2 c
      | r1 < r2   = pulley c2 r2 c1 r1 c
      | otherwise = do
            circ c1 r1 c
            circ c2 r2 c
            GL.color c
            convexShape [f,g,h,e]
   where v = c2 `sub` c1
         p = mag v
         h_theta = ( acos $ (r1 - r2) / p )
         f = setMag (rotate v h_theta) r1 `add` c1 
         g = setMag (rotate v (negate h_theta)) r1 `add` c1
         e = setMag (rotate v h_theta) r2 `add` c2 
         h = setMag (rotate v (negate h_theta)) r2 `add` c2
            


