module Update where

import Control.Lens
import SDL
import SDL.TTF
import Data.Time
import Constant
import State
import Utilities
import Block
import Collision

updateFPSCounter :: Game -> IO Game
updateFPSCounter state = do
    (newTime, fps') <- fps $ state^.fpsState.oldTime

    maybeNewFPSUpdateTime <- shouldRun (state^.fpsState.lastFPSUpdateTime)
                             fpsCounterUpdateDelay

    case maybeNewFPSUpdateTime of
        Just newFPSUpdateTime -> do
            fontSurface' <- renderUTF8Solid (state^.world.font)
                            ("FPS: " ++ show (round fps')) titaniumWhite

            let newState = state & fpsState .~ FPSCounterState newTime
                                   newFPSUpdateTime
                                 & surfaces.fontSurface .~ fontSurface'

            return newState

        Nothing -> return $ state & fpsState.oldTime .~ newTime

updateScore :: Game -> Game
updateScore state = state & score +~ 1

updatePositions :: Game -> IO Game
updatePositions state = do
    newTime <- getCurrentTime
    let updateTime x = x & lastUpdateTime .~ newTime
    if addUTCTime movementDelay (state^.lastUpdateTime) >= newTime
        then return state
        else do
            events <- pollEvents
            if windowClosed events
                then return $ state & exit .~ True
                else checkCollisions <$> (updateBlocks . updateTime $
                        updateBox state events)

updatePos :: (Num a) => Point V2 a -> a -> a -> Point V2 a
updatePos (P (V2 x y)) x' y' = P $ V2 (x + x') (y + y')

updateBox :: Game -> [Event] -> Game
updateBox state events
    | not $ null upEvents = moveBox events newState
    | otherwise = newState
    where newState = moveDown state
          upEvents = filter (`keysPressed` upKeys) events

moveBox :: [Event] -> Game -> Game
moveBox events = myRepeat (length events) moveUp

moveDown :: Game -> Game
moveDown state = state & heliPosition .~ newPos
    where newPos = updatePos (state^.heliPosition) 0 1

moveUp :: Game -> Game
moveUp state = state & heliPosition .~ newPos
    where newPos = updatePos (state^.heliPosition) 0
                    (-1 * boxMovementMultiplier)

updateBlocks :: Game -> IO Game
updateBlocks state = do
    (newUp, newDown) <- addAndRemoveBlocks state (newState^.upBlocks)
                                                 (newState^.downBlocks)
    return $ newState & upBlocks .~ newUp
             & downBlocks .~ newDown
    where newState = state & (upBlocks.traversed.position) %~
                                (\x -> updatePos x (-1) 0)
                           & (downBlocks.traversed.position) %~
                                (\x -> updatePos x (-1) 0)
