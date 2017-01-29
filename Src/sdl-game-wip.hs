{-# LANGUAGE OverloadedStrings #-}
import SDL
import SDL.Image
import SDL.TTF as Font
import Control.Monad
import Control.Lens
import Utilities
import Update

main :: IO ()
main = do
    initializeAll
    _ <- Font.init

    refreshRate' <- getMaxRefreshRate <$> getDisplays

    window <- createWindow "" defaultWindow

    screenSurface' <- getWindowSurface window
    screenSize <- surfaceDimensions screenSurface'
    surfacePixelFormat <- surfaceFormat screenSurface'

    bgSurface' <- (`convertSurface` surfacePixelFormat) =<< load bgLocation
    boxSurface' <- (`convertSurface` surfacePixelFormat) =<< load boxLocation
    ballSurface' <- (`convertSurface` surfacePixelFormat) =<< load ballLocation
    surfaceColorKey ballSurface' $= (Just $ V4 0 0 0 0)

    font' <- openFont "../Fonts/arial.ttf" fpsCounterFontSize
    fontSurface' <- renderUTF8Solid font' "0" titaniumWhite

    let surfaces' = Surfaces screenSurface' bgSurface' boxSurface' fontSurface'
                        ballSurface'
        world' = World window font' (1 / realToFrac refreshRate')
        positions' = Positions (midpoint screenSize) (midpoint screenSize)

    startState <- initialState positions' surfaces' world'

    mainLoop startState

    Font.quit

mainLoop :: Game -> IO ()
mainLoop state = do
    newState <- updateScreen =<< updatePositions state
    unless (state^.exit) (mainLoop newState)
