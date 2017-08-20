module Main
(
    main
)
where

import SDL (V2(..), quit, initializeAll, createWindow, defaultWindow,
            getWindowSurface, surfaceDimensions, surfaceFormat, convertSurface,
            pollEvents)

import SDL.Image (load)
import SDL.TTF as Font (init, quit, openFont)

import Control.Lens ((^.), (&), (.~), (+~))
import Control.Monad (when)
import Data.Time.Clock (getCurrentTime)
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, readMVar, modifyMVar_)
import Data.Text (empty)

import State (Game, Surfaces(..), World(..), dead, frames, latestFrameTime, 
              previousFrameTime, fps, score, speed)

import Constant (heliLocation, quitKeys, startKeys, fontLocation, fontSize, 
                 menuMessage, deadMessage, fpsCounterUpdateDelay, tickRate)

import Init (initialState, reInit)

import Utilities (calculateFPS, windowClosed, keysPressed, handleUpPress)

import Screen (writeStaticScreen, updateScreen)
import Update (updateHeli, addAndRemoveBlocks, moveBlocks)
import Collision (checkCollisions)

import Paths_helicopter_game (getDataFileName)

main :: IO ()
main = do
    initializeAll
    _ <- Font.init

    window <- createWindow empty defaultWindow

    screenSurface' <- getWindowSurface window
    (V2 width height) <- surfaceDimensions screenSurface'
    surfacePixelFormat <- surfaceFormat screenSurface'

    heliSurface' <- (`convertSurface` surfacePixelFormat) =<< load
                 =<< getDataFileName heliLocation

    fonty <- getDataFileName fontLocation
    font' <- openFont fonty fontSize

    let surfaces' = Surfaces screenSurface' heliSurface'

        world' = World window font' (fromIntegral width) (fromIntegral height)

    stateMVar <- initialState surfaces' world'

    mainLoop stateMVar

    Font.quit
    SDL.quit

mainLoop :: MVar Game -> IO ()
mainLoop stateMVar = do
    writeStaticScreen stateMVar menuMessage

    proceed <- menuLoop

    when proceed $ do
        fpsThread <- forkIO $ updateFPS stateMVar
        tickThread <- forkIO $ doTick stateMVar

        proceed' <- eventPollLoop

        when proceed' $ do

            killThread fpsThread 
            killThread tickThread

            writeStaticScreen stateMVar deadMessage
            deadLoop stateMVar

    where eventPollLoop = do
            reDrawScreen stateMVar 

            events <- pollEvents
            state <- readMVar stateMVar

            handleUpPress stateMVar events

            if state^.dead
                then return True
                else if windowClosed events
                    then return False
                    else eventPollLoop

updateFPS :: MVar Game -> IO ()
updateFPS stateMVar = do
    modifyMVar_ stateMVar $ \state -> do
        let frame1 = state^.frames.latestFrameTime
            frame2 = state^.frames.previousFrameTime

        return $ state & frames.fps .~ calculateFPS frame1 frame2

    threadDelay fpsCounterUpdateDelay
    updateFPS stateMVar

doTick :: MVar Game -> IO ()
doTick stateMVar = do
    modifyMVar_ stateMVar update

    threadDelay tickRate

    doTick stateMVar
    where update state = do
            state' <- addAndRemoveBlocks state
            let state'' = moveBlocks . checkCollisions $ updateHeli state'
            return $ (\s -> s & score +~ 1
                              & speed +~ 0.001) state''

reDrawScreen :: MVar Game -> IO ()
reDrawScreen stateMVar = modifyMVar_ stateMVar $ \state -> do
    updateScreen state

    time <- getCurrentTime

    return $ state & frames.previousFrameTime .~ state^.frames.latestFrameTime
                   & frames.latestFrameTime .~ time

deadLoop :: MVar Game -> IO ()
deadLoop stateMVar = do
    events <- pollEvents
    if any (`keysPressed` startKeys) events
        then reInit stateMVar >> mainLoop stateMVar
        else if any (`keysPressed` quitKeys) events || windowClosed events
            then return ()
            else deadLoop stateMVar

menuLoop :: IO Bool
menuLoop = do
    events <- pollEvents
    if any (`keysPressed` startKeys) events
        then return True
        else if windowClosed events
            then return False
            else menuLoop
