{-# LANGUAGE OverloadedStrings #-}
import SDL
import SDL.Image
import SDL.TTF as Font
import Control.Lens
import Control.Monad
import Utilities
import Update
import Foreign.C
import Data.Time

main :: IO ()
main = do
    initializeAll
    _ <- Font.init

    refreshRate' <- getMaxRefreshRate <$> getDisplays

    window <- createWindow "" defaultWindow

    screenSurface' <- getWindowSurface window
    screenSize'@(V2 width height) <- surfaceDimensions screenSurface'
    surfacePixelFormat <- surfaceFormat screenSurface'

    bgSurface' <- (`convertSurface` surfacePixelFormat) =<< load bgLocation
    boxSurface' <- (`convertSurface` surfacePixelFormat) =<< load boxLocation
    blockSurface' <- (`convertSurface` surfacePixelFormat)
                  =<< load blockLocation
    menuBGSurface' <- load menuBGLocation
    deadBGSurface' <- load deadBGLocation

    font' <- openFont "../Fonts/arial.ttf" fpsCounterFontSize
    fontSurface' <- renderUTF8Solid font' "0" titaniumWhite

    let surfaces' = Surfaces screenSurface' bgSurface' boxSurface' fontSurface'
                             blockSurface' menuBGSurface' deadBGSurface'
        world' = World window font' (1 / realToFrac refreshRate') width height
                       screenSize'

    startState <- initialState (midpoint screenSize') surfaces' world'

    writeStaticScreen startState menuBGSurface menuMessage

    exit' <- menuLoop startState
    unless exit' $ do
        mainLoop startState

        Font.quit
        SDL.quit

mainLoop :: Game -> IO ()
mainLoop state = do
    newState <- updateScreen =<< updatePositions state
    unless (newState^.exit) $
        if newState^.dead
            then do
                writeStaticScreen newState deadBGSurface deadMessage
                deadLoop newState
            else mainLoop newState

deadLoop :: Game -> IO ()
deadLoop state = do
    events <- pollEvents
    if any (`keysPressed` startKeys) events
        then do
            newState <- reInit state
            mainLoop newState
        else unless (any (`keysPressed` quitKeys) events) $ deadLoop state

menuLoop :: Game -> IO Bool
menuLoop state = do
    events <- pollEvents
    if any (`keysPressed` startKeys) events
        then return False
        else if windowClosed events
            then return True
            else menuLoop state

initialState :: Point V2 CInt -> Surfaces -> World -> IO Game
initialState heliPosition' surfaces' world' = do
    time' <- getCurrentTime
    let lastFPSUpdate = addUTCTime (-fpsCounterUpdateDelay) time'
        fpsState' = FPSCounterState time' lastFPSUpdate
    upBlocks' <- genBlocks True world'
    downBlocks' <- genBlocks False world'
    return $ Game fpsState'
                  world'
                  surfaces'
                  upBlocks'
                  downBlocks'
                  heliPosition'
                  time'
                  time'
                  0
                  False
                  False

genBlocks :: Bool -> World -> IO [Block]
genBlocks up world' = genBlocks' neededBlocks []
    {- 1 more block than width, so as they slide off the screen they don't 
    teleport on / off the screen -}
    where neededBlocks = 1 + (world'^.screenWidth) `div` blockWidth
          genBlocks' (-1) final = return final
          genBlocks' n x = do
            block'' <- block' n
            genBlocks' (n-1) (block'' : x)
          block' n = do
            height <- randomBlockHeight
            return $ makeBlock world' height (n*blockWidth) up

reInit :: Game -> IO Game
reInit state = initialState (midpoint (state^.world.screenSize))
                            (state^.surfaces) (state^.world)
