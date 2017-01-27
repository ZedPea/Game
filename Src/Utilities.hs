module Utilities
(
    windowClosed,
    fps,
    shouldRun,
    fpsCounterUpdateDelay,
    titaniumWhite,
    fpsCounterFontSize
)
where

import Data.Time
import SDL
import qualified SDL.Raw as Raw

windowClosed :: [Event] -> Bool
windowClosed [] = False
windowClosed (x:xs) = case eventPayload x of
    WindowClosedEvent _ -> True
    _ -> windowClosed xs

fps :: UTCTime -> IO (UTCTime, Double)
fps oldTime = do
    newTime <- getCurrentTime
    let interval = diffUTCTime newTime oldTime
        fps' = 1 / realToFrac interval
    return (newTime, fps')

shouldRun :: UTCTime -> NominalDiffTime -> IO (Maybe UTCTime)
shouldRun lastUpdated interval = do
    newTime <- getCurrentTime
    if addUTCTime interval lastUpdated >= newTime
        then return Nothing
        else return $ Just newTime

fpsCounterUpdateDelay :: NominalDiffTime
--seconds between the fps counter updates
fpsCounterUpdateDelay = 1

titaniumWhite :: Raw.Color
titaniumWhite = Raw.Color 255 255 255 0

fpsCounterFontSize :: Int
fpsCounterFontSize = 32
