{-# LANGUAGE OverloadedStrings #-}
import SDL
import SDL.Image
import qualified SDL.TTF as Font
import Control.Monad (unless)
import Data.Time
import Utilities

main :: IO ()
main = do
    --init SDL and Font
    initializeAll
    _ <- Font.init

    window <- createWindow "" defaultWindow

    screenSurface <- getWindowSurface window
    surfacePixelFormat <- surfaceFormat screenSurface

    imageSurface <- load "../Assets/background.jpg"
    optimizedSurface <- convertSurface imageSurface surfacePixelFormat
    freeSurface imageSurface

    font <- Font.openFont "../Fonts/arial.ttf" fpsCounterFontSize
    
    let mainLoop oldTime lastFPSUpdate = do
        events <- pollEvents
        (newTime, fps') <- fps oldTime

        maybeNewFPSUpdateTime <- shouldRun lastFPSUpdate fpsCounterUpdateDelay
        case maybeNewFPSUpdateTime of
            Just newFPSUpdateTime -> do
                fontSurface <- Font.renderUTF8Solid
                               font
                               (show (round fps' :: Int))
                               titaniumWhite

                --only redraw the window if it's time to update fps counter
                --have to redraw background otherwise fps numbers would collide
                surfaceBlit optimizedSurface Nothing screenSurface Nothing
                surfaceBlit fontSurface Nothing screenSurface Nothing
                freeSurface fontSurface

                updateWindowSurface window
                unless (windowClosed events)
                       (mainLoop newTime newFPSUpdateTime)

            Nothing -> unless (windowClosed events)
                              (mainLoop newTime lastFPSUpdate)

    time' <- getCurrentTime
    mainLoop time' (addUTCTime (- fpsCounterUpdateDelay) time')
    
    freeSurface optimizedSurface
    freeSurface screenSurface
    Font.quit
