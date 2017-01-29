{-# LANGUAGE OverloadedStrings #-}
import SDL
import SDL.Image
import SDL.TTF as Font
import Control.Lens
import Control.Monad (unless)
import Utilities

main :: IO ()
main = do
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

    font' <- openFont "../Fonts/arial.ttf" fpsCounterFontSize
    fontSurface' <- renderUTF8Solid font' "0" titaniumWhite

    let surfaces' = Surfaces screenSurface' optimizedBGSurface
                             optimizedBoxSurface fontSurface'

    startState <- initialState screenSize window font' surfaces'

    mainLoop startState

    Font.quit

mainLoop :: Game -> IO ()
mainLoop s = do
    events <- pollEvents
    newState <- updateBox s events
    newState' <- updateFPSCounter newState
    unless (windowClosed events) (mainLoop newState')

updateBox :: Game -> [Event] -> IO Game
updateBox s events
    | any wasdPressed events = do
        let newPos = moveBox events $ s^.boxPosition
        updateScreen s newPos
        return $ s & boxPosition .~ newPos
    | otherwise = return s

updateFPSCounter :: Game -> IO Game
updateFPSCounter s = do
    (newTime, fps') <- fps $ s^.fpsState.oldTime

    maybeNewFPSUpdateTime <- shouldRun (s^.fpsState.lastFPSUpdateTime)
                             fpsCounterUpdateDelay

    case maybeNewFPSUpdateTime of
        Just newFPSUpdateTime -> do
            fontSurface' <- renderUTF8Solid (s^.font) (show $ round fps')
                            titaniumWhite

            let newState = s & fpsState .~ FPSCounterState newTime
                                newFPSUpdateTime
                             & surfaces.fontSurface .~ fontSurface'

            updateScreen newState $ s^.boxPosition
            return newState

        Nothing -> return $ s & fpsState.oldTime .~ newTime
