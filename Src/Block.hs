module Block where

import SDL
import Foreign.C
import Control.Lens
import System.Random
import Data.Tuple
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

makeRectangle :: CInt -> Rectangle CInt
makeRectangle blockHeight = Rectangle (P $ V2 0 0) (V2 blockWidth blockHeight)

makeUpAndDownBlock :: Game -> CInt -> IO (Block, Block)
makeUpAndDownBlock state startWidth = do
    (upHeight, downHeight) <- randomHeights state
    let startHeight = (state^.world.screenHeight) - upHeight
        up = Block (makeRectangle upHeight) (P $ V2 startWidth startHeight)
        down = Block (makeRectangle downHeight) (P $ V2 startWidth 0)
    return (up, down)

addBlocks :: Game -> [Block] -> [Block] -> IO ([Block], [Block])
addBlocks state up down
    | length up + length down == 0 = do
        (newUp, newDown) <- makeUpAndDownBlock state startWidth
        return ([newUp], [newDown])
    | otherwise = do
        (newUp, newDown) <- makeUpAndDownBlock state (x + blockWidth)
        return (up ++ [newUp], down ++ [newDown])
    where neededBlocks = 1 + (state^.world.screenWidth) `div` blockWidth
          startWidth = fromIntegral $ neededBlocks * blockWidth
          (P (V2 x _)) = last up^.position

randomHeights :: Game -> IO (CInt, CInt)
randomHeights state
    | length (state^.upBlocks) + length (state^.downBlocks) == 0 = do
        first <- getStdRandom (randomR (0, scrHeight - heliHeight))
        second <- getStdRandom (randomR (0, scrHeight - first -
                                         (heliHeight * 3)))
        return (first, second)
    | otherwise = do
        let (Rectangle (P (V2 _ _)) (V2 _ upHeight))
                = last (state^.upBlocks)^.size
            (Rectangle (P (V2 _ _)) (V2 _ downHeight))
                = last (state^.downBlocks)^.size
        up <- getStdRandom (randomR (True, False))
        if up
            then gen scrHeight upHeight downHeight
            else swap <$> gen scrHeight downHeight upHeight
    where scrHeight = state^.world.screenHeight

gen :: CInt -> CInt -> CInt -> IO (CInt, CInt)
gen scrHeight firstHeight secondHeight = do
    let firstMaxHeight = clamp firstHeight $ scrHeight - secondHeight
                                                       - (heliHeight * 3)
        secondMaxHeight = scrHeight - firstMaxHeight - (heliHeight * 3)
    first <- getStdRandom (randomR (0, firstMaxHeight))
    second <- getStdRandom (randomR (0, clamp secondHeight secondMaxHeight))
    return (first, second)
