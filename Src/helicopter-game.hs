{-# LANGUAGE OverloadedStrings #-}
import SDL
import SDL.Image
import SDL.TTF as Font
import Control.Monad
import State
import Loop
import Constant
import Display
import Init
import Utilities
import Screen
import Paths_helicopter_game

main :: IO ()
main = do
    initializeAll
    _ <- Font.init

    refreshRate' <- getMaxRefreshRate <$> getDisplays

    window <- createWindow "" defaultWindow

    screenSurface' <- getWindowSurface window
    screenSize'@(V2 width height) <- surfaceDimensions screenSurface'
    surfacePixelFormat <- surfaceFormat screenSurface'

    bgSurface' <- (`convertSurface` surfacePixelFormat) =<< load
               =<< getDataFileName bgLocation
    heliSurface' <- (`convertSurface` surfacePixelFormat) =<< load
                 =<< getDataFileName heliLocation
    blockSurface' <- (`convertSurface` surfacePixelFormat) =<< load
                  =<< getDataFileName blockLocation
    menuBGSurface' <- load =<< getDataFileName menuBGLocation
    deadBGSurface' <- load =<< getDataFileName deadBGLocation

    fonty <- getDataFileName fontLocation
    font' <- openFont fonty fontSize
    fontSurface' <- renderUTF8Solid font' "0" titaniumWhite

    let surfaces' = Surfaces screenSurface' bgSurface' heliSurface'
                             fontSurface' blockSurface' menuBGSurface'
                             deadBGSurface'
        world' = World window font' (1 / realToFrac refreshRate') width height
                       screenSize'

    startState <- initialState (heliPosInit $ midpoint screenSize') surfaces'
                  world'

    writeStaticScreen startState menuBGSurface menuMessage

    exit' <- menuLoop startState
    unless exit' $ mainLoop startState

    Font.quit
    SDL.quit
