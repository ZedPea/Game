module Update
(
    updateHeli,
    addAndRemoveBlocks,
    moveBlocks
)
where

import Control.Lens ((&), (^.), (+~), traversed, (-~), both)
import Data.List (genericLength)

import Constant (heliMovementMultiplier, blockWidth)
import State (Game, heliPosition, world, screenWidth, blocks, blockPairs,
              bottomLeft, bottomRight, topLeft, topRight, x, y, speed,
              upPressed)

import Block (addBlockPair, removeBlockPair)

updateHeli :: Game -> Game
updateHeli state
    | state^.upPressed = moveHeliUp state
    | otherwise = moveHeliDown state

moveHeliUp :: Game -> Game
moveHeliUp = moveHeli (-1 * heliMovementMultiplier)

moveHeliDown :: Game -> Game
moveHeliDown = moveHeli 1

moveHeli :: Double -> Game -> Game
moveHeli n state = state & moveHeli' bottomLeft
                         & moveHeli' bottomRight
                         & moveHeli' topLeft
                         & moveHeli' topRight
    where moveHeli' pos = heliPosition.pos.y +~ n

moveBlocks :: Game -> Game
moveBlocks state = 
    state & moveBlock bottomLeft
          & moveBlock bottomRight
          & moveBlock topLeft
          & moveBlock topRight
    where moveBlock pos = blocks.blockPairs.traversed.both.pos.x -~ 
                          (state^.speed)

addAndRemoveBlocks :: Game -> IO Game
addAndRemoveBlocks state
    | not maxBlocks = addBlockPair state
    | blockOffScreen = removeBlockPair <$> addBlockPair state
    | otherwise = return state
    where neededBlocks = 1 + state^.world.screenWidth / blockWidth
          numBlocks = genericLength $ state^.blocks.blockPairs
          maxBlocks = numBlocks > neededBlocks 
          (firstUpBlock, _) = head $ state^.blocks.blockPairs
          blockOffScreen = firstUpBlock^.bottomRight.x <= 0
