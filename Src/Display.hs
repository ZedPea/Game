module Display where

import SDL
import Foreign.C

getPrimaryDisplay :: [Display] -> Maybe Display
getPrimaryDisplay [] = Nothing
getPrimaryDisplay (x:xs)
    | displayBoundsPosition x == P (V2 0 0) = Just x
    | otherwise = getPrimaryDisplay xs

getMaxRefreshRate :: [Display] -> CInt
getMaxRefreshRate displays
    --unspecified or not found
    | null refreshRates || maximum refreshRates == 0 = 60
    | otherwise = maximum refreshRates
    where refreshRates = map highestRefreshRate displays

highestRefreshRate :: Display -> CInt
highestRefreshRate display = maximum refreshRates
    where resolution = displayBoundsSize display
          currentRes x = displayModeSize x == resolution
          validDisplayModes = filter currentRes $ displayModes display
          refreshRates = map displayModeRefreshRate validDisplayModes
