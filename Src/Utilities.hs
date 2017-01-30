{-# LANGUAGE TemplateHaskell #-}
module Utilities where

import Data.Time
import SDL
import SDL.TTF.FFI
import Foreign.C
import Control.Lens
import qualified SDL.Raw as Raw

data Game = Game {
    _fpsState :: FPSCounterState,
    _world :: World,
    _surfaces :: Surfaces,
    _positions :: Positions,
    _lastUpdateTime :: UTCTime,
    _lastScreenUpdateTime :: UTCTime,
    _exit :: Bool
}

data FPSCounterState = FPSCounterState {
    _oldTime :: UTCTime,
    _lastFPSUpdateTime :: UTCTime
}

data Surfaces = Surfaces {
    _screenSurface :: Surface,
    _bgSurface :: Surface,
    _boxSurface :: Surface,
    _fontSurface :: Surface
}

data World = World {
    _mainWindow :: Window,
    _font :: TTFFont,
    _refreshRate :: NominalDiffTime
}

data Positions = Positions {
    _boxPosition :: Point V2 CInt
}

makeLenses ''Game
makeLenses ''FPSCounterState
makeLenses ''Surfaces
makeLenses ''World
makeLenses ''Positions

initialState :: Positions -> Surfaces -> World -> IO Game
initialState positions' surfaces' world' = do
    time' <- getCurrentTime
    let lastFPSUpdate = addUTCTime (-fpsCounterUpdateDelay) time'
        fpsState' = FPSCounterState time' lastFPSUpdate
    return $ Game fpsState'
                  world'
                  surfaces'
                  positions'
                  time'
                  time'
                  False

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

boxMovementMultiplier :: CInt
boxMovementMultiplier = 30

upKeys :: [Scancode]
upKeys = [ScancodeW, ScancodeUp]

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

fpsCounterUpdateDelay :: NominalDiffTime
--seconds between the fps counter updates
fpsCounterUpdateDelay = 1

movementDelay :: NominalDiffTime
--seconds between game logic updates, i.e. moving objects etc
movementDelay = 0.01

titaniumWhite :: Raw.Color
titaniumWhite = Raw.Color 255 255 255 0

fpsCounterFontSize :: Int
fpsCounterFontSize = 32

midpoint :: (Integral a) => V2 a -> Point V2 a
midpoint (V2 x y) = P $ V2 (x `div` 2) (y `div` 2)

bgLocation :: String
bgLocation = "../Assets/background.jpg"

boxLocation :: String
boxLocation = "../Assets/box.jpg"

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
