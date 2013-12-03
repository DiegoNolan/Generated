
module Vec2
   (
     Vec2 (..)
   , add
   , sub
   , mag
   , dot
   , angle
   , rotate
   , setMag
   , fromX
   ) where

data Vec2 = Vec2 !Float !Float deriving Show

add :: Vec2 -> Vec2 -> Vec2
add (Vec2 u1 u2) (Vec2 v1 v2) = Vec2 (u1+v1) (u2+v2)

sub :: Vec2 -> Vec2 -> Vec2
sub (Vec2 u1 u2) (Vec2 v1 v2) = Vec2 (u1-v1) (u2-v2)

mag :: Vec2 -> Float
mag (Vec2 x y) = sqrt $ x*x + y*y

dot :: Vec2 -> Vec2 -> Float
dot (Vec2 u1 u2) (Vec2 v1 v2) = u1*v1 + u2*v2

angle :: Vec2 -> Vec2 -> Float
angle u@(Vec2 u1 u2) v@(Vec2 v1 v2) =
      (signum t) * ( acos $ dot u v / ( mag u * mag v) )
   where t = u1*v2 - u2*v1

rotate :: Vec2 -> Float -> Vec2
rotate (Vec2 x y) theta = Vec2 (x * ct - y * st) (x * st + y * ct)
   where ct = cos theta
         st = sin theta

setMag :: Vec2 -> Float -> Vec2
setMag v@(Vec2 x y) nm = Vec2 ((x / m) * nm) ((y / m) * nm)
   where m = mag v

fromX :: Vec2 -> Float
fromX = angle (Vec2 1 0)

fromY :: Vec2 -> Float
fromY = angle (Vec2 0 1)

