module Loop where

import SDL
import Control.Lens
import Control.Monad
import State
import Constant
import Screen
import Update
import Utilities
import Init

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
        else unless (any (`keysPressed` quitKeys) events ||
                     windowClosed events) $ deadLoop state

menuLoop :: Game -> IO Bool
menuLoop state = do
    events <- pollEvents
    if any (`keysPressed` startKeys) events
        then return False
        else if windowClosed events
            then return True
            else menuLoop state
