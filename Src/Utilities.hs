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
    _boxPosition :: Point V2 CInt,
    _mainWindow :: Window,
    _font :: TTFFont,
    _surfaces :: Surfaces
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

makeLenses ''Game
makeLenses ''FPSCounterState
makeLenses ''Surfaces

windowClosed :: [Event] -> Bool
windowClosed [] = False
windowClosed (x:xs) = case eventPayload x of
    WindowClosedEvent _ -> True
    _ -> windowClosed xs

wasdPressed :: Event -> Bool
wasdPressed event = case eventPayload event of
    KeyboardEvent keyboardEvent ->
        keyboardEventKeyMotion keyboardEvent == Pressed &&
        keysymScancode (keyboardEventKeysym keyboardEvent) `elem` wasd
    _ -> False

moveBox :: [Event] -> Point V2 CInt -> Point V2 CInt
moveBox events oldPos = let movement = filter wasdPressed events
                        in  moveBox' (map eventPayload movement) oldPos
    where moveBox' [] finalPos = finalPos
          moveBox' (x:xs) oldPos' = moveBox' xs (updateBoxPos x oldPos')

updateBoxPos :: EventPayload -> Point V2 CInt -> Point V2 CInt
updateBoxPos (KeyboardEvent data') oldPos = case key of
    ScancodeW -> updatePos oldPos 0 ((-1) * boxMovementMultiplier)
    ScancodeS -> updatePos oldPos 0 (1 * boxMovementMultiplier)
    ScancodeD -> updatePos oldPos (1 * boxMovementMultiplier) 0
    ScancodeA -> updatePos oldPos ((-1) * boxMovementMultiplier) 0
    _ -> error "Internal error - updateBoxPos ScanCode pattern not matched"
    where key = keysymScancode $ keyboardEventKeysym data'
updateBoxPos _ _ = error "Internal error - updateBoxPos KeyboardEvent pattern\
                         \ not matched"

boxMovementMultiplier :: CInt
boxMovementMultiplier = 20

updatePos :: (Num a) => Point V2 a -> a -> a -> Point V2 a
updatePos (P (V2 x y)) x' y' = P $ V2 (x + x') (y + y')

wasd :: [Scancode]
wasd = [ScancodeW, ScancodeA, ScancodeS, ScancodeD]

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

titaniumWhite :: Raw.Color
titaniumWhite = Raw.Color 255 255 255 0

fpsCounterFontSize :: Int
fpsCounterFontSize = 32

midpoint :: (Integral a) => V2 a -> Point V2 a
midpoint (V2 x y) = P $ V2 (x `div` 2) (y `div` 2)

initialState :: V2 CInt -> Window -> TTFFont -> Surfaces -> IO Game
initialState dimensions window font' surfaces' = do
    time' <- getCurrentTime
    let lastFPSUpdate = addUTCTime (-fpsCounterUpdateDelay) time'
        boxPos = midpoint dimensions
        fpsState' = FPSCounterState time' lastFPSUpdate
    return $ Game fpsState' boxPos window font' surfaces'

writeToScreen :: Game -> ((Surface -> Const Surface Surface) -> Surfaces
                      -> Const Surface Surfaces) -> IO ()
writeToScreen s source = surfaceBlit (s^.surfaces.source) Nothing
                         (s^.surfaces.screenSurface) Nothing

--have type signatures gone too far?
writeToScreenWithPos :: Game -> ((Surface -> Const Surface Surface)
                             -> Surfaces -> Const Surface Surfaces)
                             -> Point V2 CInt -> IO ()
writeToScreenWithPos s source pos = surfaceBlit (s^.surfaces.source) Nothing
                                    (s^.surfaces.screenSurface) (Just pos)

updateScreen :: Game -> Point V2 CInt -> IO ()
updateScreen s pos = do
    writeToScreen s bgSurface 
    writeToScreenWithPos s boxSurface pos
    writeToScreen s fontSurface
    updateWindowSurface (s^.mainWindow)
