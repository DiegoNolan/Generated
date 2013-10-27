
module Vec2
   (
     Vec2
   , add
   , sub
   , mag
   , dot
   , angle
   , rotate
   , setMag
   , fromX
   ) where

type Vec2 = (Float, Float)

add :: Vec2 -> Vec2 -> Vec2
add (u1,u2) (v1,v2) = (u1+v1,u2+v2)

sub :: Vec2 -> Vec2 -> Vec2
sub (u1,u2) (v1,v2) = (u1-v1,u2-v2)

mag :: Vec2 -> Float
mag (x,y) = sqrt $ x*x + y*y

dot :: Vec2 -> Vec2 -> Float
dot (u1,u2) (v1,v2) = u1*v1 + u2*v2

angle :: Vec2 -> Vec2 -> Float
angle u@(u1,u2) v@(v1,v2) = (signum t) * ( acos $ dot u v / ( mag u * mag v) )
   where t = u1*v2 - u2*v1

rotate :: Vec2 -> Float -> Vec2
rotate (x,y) theta = ( x * ct - y * st, x * st + y * ct )
   where ct = cos theta
         st = sin theta

setMag :: Vec2 -> Float -> Vec2
setMag v@(x,y) nm = ( (x / m) * nm, (y / m) * nm )
   where m = mag v

fromX :: Vec2 -> Float
fromX = angle (1,0)

fromY :: Vec2 -> Float
fromY = angle (0,1)

