module Utilities where

import Foreign.C
import SDL
import Data.Time
import Constant
import Control.Lens
import State

minHeight :: World -> CInt
minHeight world'
    | preferred < 0 = 0
    | otherwise = preferred
    where preferred = (world'^.screenHeight) `div` 2 - (heliHeight * 3)

maxHeight :: World -> CInt
maxHeight world'
    | preferred < 0 = 0
    | otherwise = preferred
    where preferred = (world'^.screenHeight) `div` 2 - heliHeight

windowClosed :: [Event] -> Bool
windowClosed [] = False
windowClosed (x:xs) = case eventPayload x of
    WindowClosedEvent _ -> True
    _ -> windowClosed xs

keysPressed :: Event -> [Scancode] -> Bool
keysPressed event keys = case eventPayload event of
    KeyboardEvent keyboardEvent ->
        keyboardEventKeyMotion keyboardEvent == Pressed &&
        keysymScancode (keyboardEventKeysym keyboardEvent) `elem` keys
    _ -> False

fps :: UTCTime -> IO (UTCTime, Double)
fps oldTime' = do
    newTime <- getCurrentTime
    let interval = diffUTCTime newTime oldTime'
        fps' = 1 / realToFrac interval
    return (newTime, fps')

shouldRun :: UTCTime -> NominalDiffTime -> IO (Maybe UTCTime)
shouldRun lastUpdated interval = do
    newTime <- getCurrentTime
    if addUTCTime interval lastUpdated >= newTime
        then return Nothing
        else return $ Just newTime

midpoint :: (Integral a) => V2 a -> Point V2 a
midpoint (V2 x y) = P $ V2 (x `div` 2) (y `div` 2)

--give this a better name / find if there's a library function
myRepeat :: (Num a, Eq a) => a -> (b -> b) -> b -> b
myRepeat 0 _ final = final
myRepeat n f old = let new = f old
                   in  myRepeat (n-1) f new 

clamp :: (Ord a, Num a) => a -> a -> a
clamp old new
    | new < 0 = 0
    | new >= old + maxIncrement = old + maxIncrement
    | otherwise = new
