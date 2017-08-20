module Collision
(
    checkCollisions
)
where

import Control.Lens ((^.), (&), (.~))

import State (Game, Block, dead, heliPosition, world, screenHeight, topLeft,
              bottomLeft, x, y, blocks, blockPairs, topRight)

import Utilities (listOfPairsToList)

--https://stackoverflow.com/a/306332
collides :: Block -> Block -> Bool
collides blockA blockB = not $ (blockA^.topLeft.x) > (blockB^.topRight.x) ||
                               (blockA^.topRight.x) < (blockB^.topLeft.x) ||
                               (blockA^.topLeft.y) > (blockB^.bottomLeft.y) ||
                               (blockA^.bottomLeft.y) < (blockB^.topLeft.y)

checkCollisions :: Game -> Game
checkCollisions state
    | state^.heliPosition.topLeft.y < 0 || 
      state^.heliPosition.bottomLeft.y > state^.world.screenHeight || 
      collision = state & dead .~ True
    | otherwise = state
    where collision = any (collides (state^.heliPosition)) allBlocks
          allBlocks = listOfPairsToList (state^.blocks.blockPairs)
