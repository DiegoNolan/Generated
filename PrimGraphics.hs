
module PrimGraphics
   (
     DelayedGraphic
   , renderDelayed
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

-- | Delayed Graphic type -----------------------------------------------------

-- Left side is an io action to create a display list
type DelayedGraphic = Either (IO GL.DisplayList) GL.DisplayList

renderDelayed :: DelayedGraphic -> IO DelayedGraphic
renderDelayed (Left f) = f >>= (renderDelayed . Right)
renderDelayed dg@(Right d) = GL.callList d >> return dg

circle_sides :: Int
circle_sides = 50

--  conversions to screen coordinates
toSX :: Float -> GL.GLfloat
toSX = realToFrac . scaling

toSY :: Float -> GL.GLfloat
toSY y = aspectRatio * ( realToFrac $ scaling y)

-- helper function for a opengl vertex of floats
vertex :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> IO ()
vertex x y z = GL.vertex $ GL.Vertex3 x y z

-- hepler funciton for creating display lists
mkList :: IO () -> IO GL.DisplayList
mkList f = GL.defineNewList GL.Compile f

------------------------------------------------------------------------------
-- | Functions that create Delayed Graphics ----------------------------------
------------------------------------------------------------------------------
circle :: Vec2 -> Float -> Color -> DelayedGraphic
circle cent r c = Left (mkList $ circ cent r c)

square :: Vec2 -> Vec2 -> Color -> DelayedGraphic
square tl@(Vec2 x1 y1) br@(Vec2 x2 y2) col =
   Left (mkList $ do
            GL.color col
            convexShape [tl,(Vec2 x2 y1),br,(Vec2 x1 y2)]
        )
-------------------------------------------------------------------------------
-- | Functions that can be passed to the mkList function to creat Display lists
-------------------------------------------------------------------------------
circ :: Vec2 -> Float -> Color -> IO ()
circ (Vec2 xrel yrel) r c = do
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
         conv f@(Vec2 x1 y1) l@(Vec2 x2 y2) ((r@(Vec2 x3 y3)):rest) = do
            vertex (toSX x1) (toSY y1) 0
            vertex (toSX x2) (toSY y2) 0
            vertex (toSX x3) (toSY y3) 0
            conv f r rest
convexShape _  = error "Too few points"

triangleStrip :: [Vec2] -> IO ()
triangleStrip xs = GL.renderPrimitive GL.Triangles $ triStrip xs
   where triStrip ((Vec2 x1 y1):(Vec2 x2 y2):(Vec2 x3 y3):rest) = do
            vertex (toSX x1) (toSY y1) 0
            vertex (toSX x2) (toSY y2) 0
            vertex (toSX x3) (toSY y3) 0
            triStrip ((Vec2 x2 y2):(Vec2 x3 y3):rest)
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
            


