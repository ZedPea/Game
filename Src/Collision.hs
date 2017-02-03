module Collision where

import SDL
import Foreign.C
import Control.Lens
import Constant
import Block
import State

checkCollisions :: Game -> Game
checkCollisions state
    | collision = state & dead .~ True
    | otherwise = state
    where (P (V2 x y)) = state^.heliPosition
          heli = Rectangle (P $ V2 x y)
                           (V2 (x + heliSize) (y + heliSize))
          collision = any (collides heli) (blockRectangles state)

collides :: Rectangle CInt -> Rectangle CInt -> Bool
collides (Rectangle (P (V2 a b)) (V2 c d))
         (Rectangle (P (V2 a' b')) (V2 c' d')) =
         not $ d < b' || b > d' || c < a' || a > c
