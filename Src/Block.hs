module Block where

import SDL
import Foreign.C
import Control.Lens
import System.Random
import State
import Constant
import Utilities

blockRectangles :: Game -> [Rectangle CInt]
blockRectangles state = map alterRectangle blocks
    where blocks = state^.upBlocks ++ state^.downBlocks

alterRectangle :: Block -> Rectangle CInt
alterRectangle block = Rectangle (P (V2 (a + x) (b + y)))
                                 (V2 (c + x) (d + y))
    where (P (V2 x y)) = block^.position
          (Rectangle (P (V2 a b)) (V2 c d)) = block^.size


addAndRemoveBlocks :: Game -> [Block] -> [Block] -> IO ([Block], [Block])
addAndRemoveBlocks state up down
    | blockLen < 2 * fromIntegral neededBlocks = addBlocks state up down
    | x <= -blockWidth = addBlocks state (tail up) (tail down)
    | otherwise = return (up, down)
    where firstUp = head up
          (P (V2 x _)) = firstUp^.position
          neededBlocks = 1 + state^.world.screenWidth `div` blockWidth
          blockLen = length up + length down

addBlocks :: Game -> [Block] -> [Block] -> IO ([Block], [Block])
addBlocks state up down = do
    height <- randomBlockHeight (state^.world)
    height2 <- randomBlockHeight (state^.world)
    if length up + length down == 0
        then let newUp = makeBlock (state^.world) 
                         (fromIntegral $ neededBlocks * blockWidth) height
                         True
                 newDown = makeBlock (state^.world)
                           (fromIntegral $ neededBlocks * blockWidth) height2
                           False
             in  return (up ++ [newUp], down ++ [newDown])
        else let newUp = makeBlock (state^.world) x' height True
                 newDown = makeBlock (state^.world) x' height2 False
             in  return (up ++ [newUp], down ++ [newDown])
    where (P (V2 x _)) = last up^.position
          x' = x + blockWidth
          neededBlocks = 1 + (state^.world.screenWidth) `div` blockWidth

makeBlock :: World -> CInt -> CInt -> Bool -> Block
makeBlock world' startWidth height up
    | up = Block (makeRectangle height) (P $ V2 startWidth startHeight)
    | otherwise = Block (makeRectangle height) (P $ V2 startWidth 0)
    where startHeight = (world'^.screenHeight) - height

makeRectangle :: CInt -> Rectangle CInt
makeRectangle blockHeight = Rectangle (P $ V2 0 0) (V2 blockWidth blockHeight)

randomBlockHeight :: World -> IO CInt
randomBlockHeight world' = getStdRandom (randomR (minHeight world',
                                                  maxHeight world'))
