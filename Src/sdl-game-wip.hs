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

    bgSurface' <- (`convertSurface` surfacePixelFormat) =<< load bgLocation
    boxSurface' <- (`convertSurface` surfacePixelFormat) =<< load boxLocation

    font' <- openFont "../Fonts/arial.ttf" fpsCounterFontSize
    fontSurface' <- renderUTF8Solid font' "0" titaniumWhite

    let surfaces' = Surfaces screenSurface' bgSurface' boxSurface' fontSurface'

    startState <- initialState screenSize window font' surfaces'

    mainLoop startState

    Font.quit

mainLoop :: Game -> IO ()
mainLoop state = do
    events <- pollEvents
    newState <- updateFPSCounter =<< updateBox state events
    unless (windowClosed events) (mainLoop newState)

updateBox :: Game -> [Event] -> IO Game
updateBox state events
    | any wasdPressed events = do
        let newPos = moveBox events $ state^.boxPosition
        updateScreen state newPos
        return $ state & boxPosition .~ newPos
    | otherwise = return state

updateFPSCounter :: Game -> IO Game
updateFPSCounter state = do
    (newTime, fps') <- fps $ state^.fpsState.oldTime

    maybeNewFPSUpdateTime <- shouldRun (state^.fpsState.lastFPSUpdateTime)
                             fpsCounterUpdateDelay

    case maybeNewFPSUpdateTime of
        Just newFPSUpdateTime -> do
            fontSurface' <- renderUTF8Solid (state^.font) (show $ round fps')
                            titaniumWhite

            let newState = state & fpsState .~ FPSCounterState newTime
                                   newFPSUpdateTime
                                 & surfaces.fontSurface .~ fontSurface'

            updateScreen newState $ state^.boxPosition
            return newState

        Nothing -> return $ state & fpsState.oldTime .~ newTime
