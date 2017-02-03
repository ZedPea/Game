module Init where

import SDL
import Foreign.C
import Data.Time
import Control.Lens
import State
import Constant
import Utilities

initialState :: Point V2 CInt -> Surfaces -> World -> IO Game
initialState heliPosition' surfaces' world' = do
    time' <- getCurrentTime
    let lastFPSUpdate = addUTCTime (-fpsCounterUpdateDelay) time'
        fpsState' = FPSCounterState time' lastFPSUpdate
    return $ Game fpsState' world' surfaces' [] [] heliPosition' time' time'
                  0 False False

reInit :: Game -> IO Game
reInit state = initialState heliPos (state^.surfaces) (state^.world)
    where heliPos = heliPosInit $ midpoint (state^.world.screenSize)

heliPosInit :: Point V2 CInt -> Point V2 CInt
heliPosInit (P (V2 width height)) = P $ V2 (width - (heliSize `div` 2))
                                           (height - (heliSize `div` 2))
