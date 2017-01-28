{-# LANGUAGE OverloadedStrings #-}
import SDL
import SDL.Image
import qualified SDL.TTF as Font
import Control.Monad (unless)
import Utilities

main :: IO ()
main = do
    --init SDL and Font
    initializeAll
    _ <- Font.init

    window <- createWindow "" defaultWindow

    screenSurface' <- getWindowSurface window
    screenSize <- surfaceDimensions screenSurface'
    surfacePixelFormat <- surfaceFormat screenSurface'

    bgSurface' <- load "../Assets/background.jpg"
    optimizedBGSurface <- convertSurface bgSurface' surfacePixelFormat
    freeSurface bgSurface'

    boxSurface' <- load "../Assets/box.jpg"
    optimizedBoxSurface <- convertSurface boxSurface' surfacePixelFormat
    freeSurface boxSurface'

    font' <- Font.openFont "../Fonts/arial.ttf" fpsCounterFontSize
    fontSurface' <- Font.renderUTF8Solid font' "0" titaniumWhite

    let surfaces = Surfaces screenSurface' optimizedBGSurface
                            optimizedBoxSurface fontSurface'

    startState <- initialState screenSize window font'

    --enter main loop
    mainLoop startState surfaces
    
    freeSurface optimizedBGSurface
    freeSurface optimizedBoxSurface
    freeSurface screenSurface'
    Font.quit

mainLoop :: State -> Surfaces -> IO ()
mainLoop s surfaces = do
    events <- pollEvents
    newState <- updateBox s events surfaces
    (newState', newSurfaces) <- updateFPSCounter newState surfaces
    unless (windowClosed events) (mainLoop newState' newSurfaces)

updateBox :: State -> [Event] -> Surfaces -> IO State
updateBox state events s
    | any wasdPressed events = do
        let newPos = moveBox events (boxPosition state)
        surfaceBlit (bgSurface s) Nothing (screenSurface s) Nothing
        surfaceBlit (boxSurface s) Nothing (screenSurface s) (Just newPos)
        surfaceBlit (fontSurface s) Nothing (screenSurface s) Nothing
        updateWindowSurface (mainWindow state)
        return $ state { boxPosition = newPos }
    | otherwise = return state

updateFPSCounter :: State -> Surfaces -> IO (State, Surfaces)
updateFPSCounter oldState@(State f boxPos window font') s = do
    (newTime, fps') <- fps (oldTime f)
    maybeNewFPSUpdateTime <- shouldRun (lastFPSUpdateTime f)
                             fpsCounterUpdateDelay
    case maybeNewFPSUpdateTime of
        Just newFPSUpdateTime -> do
            fontSurface' <- Font.renderUTF8Solid font'
                           (show (round fps' :: Int)) titaniumWhite

            surfaceBlit (bgSurface s) Nothing (screenSurface s) Nothing
            surfaceBlit (boxSurface s) Nothing (screenSurface s) (Just boxPos)
            surfaceBlit fontSurface' Nothing (screenSurface s) Nothing

            updateWindowSurface window

            freeSurface (fontSurface s)


            return (oldState { fpsState = FPSCounterState newTime
                               newFPSUpdateTime },
                           s { fontSurface = fontSurface' } )

        Nothing -> return (oldState { fpsState = FPSCounterState newTime
                                      (lastFPSUpdateTime f) }, s)
