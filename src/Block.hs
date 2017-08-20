module Block
(
    addBlockPair,
    removeBlockPair
)
where

import Control.Lens ((^.), (&), (.~), (-~), (+~))
import System.Random (getStdRandom, randomR)

import State (Game, Block(..), Direction(..), Coord(..), world, blocks,
              x, y, screenWidth, screenHeight, blockPairs, trend, bottomRight,
              topLeft, topRight, bottomLeft)

import Constant (blockWidth, blockIncrement, blockGap)

addBlockPair :: Game -> IO Game
addBlockPair state = do
    threshold <- (> 0.8) <$> (getStdRandom $ randomR (0.0, 1.0) :: IO Double)

    let newState = state & blocks.trend .~ if threshold 
                                            then flipTrend trend'
                                            else trend'

    --add new blocks to the end
    return $ newState & blocks.blockPairs .~ 
                        (oldBlocks ++ [genBlockPair newState])

    where flipTrend Up = Down
          flipTrend Down = Up
          trend' = state^.blocks.trend
          oldBlocks = state^.blocks.blockPairs

removeBlockPair :: Game -> Game
removeBlockPair state = state & blocks.blockPairs .~ 
                                tail (state^.blocks.blockPairs)

genBlockPair :: Game -> (Block, Block)
genBlockPair state
    | null (state^.blocks.blockPairs) = startBlockPair state
    | isUp (state^.blocks.trend) = (moveBottomBlockUp state lastBottomBlock,
                                    moveTopBlockUp state lastTopBlock)
    | otherwise = (moveBottomBlockDown state lastBottomBlock,
                   moveTopBlockDown state lastTopBlock)
    where (lastBottomBlock, lastTopBlock) = last $ state^.blocks.blockPairs
          isUp Up = True
          isUp Down = False

moveTopBlockUp :: Game -> Block -> Block
moveTopBlockUp _ oldBlock
    | oldBlock^.bottomLeft.y - blockIncrement < 0 = alterWidth oldBlock
    | otherwise = alterWidth oldBlock & bottomLeft.y -~ blockIncrement
                                      & bottomRight.y -~ blockIncrement

moveTopBlockDown :: Game -> Block -> Block
moveTopBlockDown state oldBlock
    | newBlockBottom < blockGap = alterWidth oldBlock
    | otherwise = alterWidth oldBlock & bottomLeft.y +~ blockIncrement
                                      & bottomRight.y +~ blockIncrement
    where newBlockBottom = state^.world.screenHeight - oldBlock^.bottomLeft.y
                                                     - blockIncrement

moveBottomBlockUp :: Game -> Block -> Block
moveBottomBlockUp _ oldBlock
    | oldBlock^.topLeft.y + blockIncrement < blockGap = alterWidth oldBlock
    | otherwise = alterWidth oldBlock & topLeft.y -~ blockIncrement
                                      & topRight.y -~ blockIncrement

moveBottomBlockDown :: Game -> Block -> Block
moveBottomBlockDown state oldBlock
    | newBlockTop > state^.world.screenHeight = alterWidth oldBlock
    | otherwise = alterWidth oldBlock & topLeft.y +~ blockIncrement
                                      & topRight.y +~ blockIncrement
    where newBlockTop = oldBlock^.topLeft.y - blockIncrement

alterWidth :: Block -> Block
alterWidth old = old & bottomLeft.x +~ blockWidth
                     & bottomRight.x +~ blockWidth
                     & topLeft.x +~ blockWidth
                     & topRight.x +~ blockWidth

startBlockPair :: Game -> (Block, Block)
startBlockPair state = 
    (Block (Coord (state^.world.screenWidth)
                  (state^.world.screenHeight))

           (Coord (state^.world.screenWidth + blockWidth) 
                  (state^.world.screenHeight))

           (Coord (state^.world.screenWidth) 
                  (state^.world.screenHeight - height))

           (Coord (state^.world.screenWidth + blockWidth) 
                  (state^.world.screenHeight - height)),

     Block (Coord (state^.world.screenWidth) height)

           (Coord (state^.world.screenWidth + blockWidth) height)

           (Coord (state^.world.screenWidth) 0)

           (Coord (state^.world.screenWidth + blockWidth) 0))

    where height = (state^.world.screenHeight - blockGap) / 2
