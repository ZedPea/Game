module Update where

import Utilities
import SDL
import SDL.TTF
import Control.Lens
import Foreign.C
import Data.Time

updateBoxPos :: Point V2 CInt -> Point V2 CInt
updateBoxPos oldPos = updatePos oldPos 0 $ (-1) * boxMovementMultiplier

moveBox :: [Event] -> Point V2 CInt -> Point V2 CInt
moveBox events = myRepeat upEvents updateBoxPos
    where upEvents = length $ filter (`keysPressed` upKeys) events

--give this a better name / find if there's a library function
myRepeat :: (Num a, Eq a) => a -> (b -> b) -> b -> b
myRepeat 0 _ final = final
myRepeat n f old = let new = f old
                   in  myRepeat (n-1) f new 

updatePos :: (Num a) => Point V2 a -> a -> a -> Point V2 a
updatePos (P (V2 x y)) x' y' = P $ V2 (x + x') (y + y')

updateBox :: Game -> [Event] -> IO Game
updateBox state events
    | any (`keysPressed` upKeys) events = do
        let newPos = moveBox events $ state^.positions.boxPosition
        return $ state & positions.boxPosition .~ newPos
    | otherwise = return state

updateBall :: Game -> IO Game
updateBall state = return $ state & positions.ballPosition .~ newPos 
    where newPos = updatePos (state^.positions.ballPosition) 1 1
    
updateFPSCounter :: Game -> IO Game
updateFPSCounter state = do
    (newTime, fps') <- fps $ state^.fpsState.oldTime

    maybeNewFPSUpdateTime <- shouldRun (state^.fpsState.lastFPSUpdateTime)
                             fpsCounterUpdateDelay

    case maybeNewFPSUpdateTime of
        Just newFPSUpdateTime -> do
            fontSurface' <- renderUTF8Solid (state^.world.font)
                            (show $ round fps') titaniumWhite

            let newState = state & fpsState .~ FPSCounterState newTime
                                   newFPSUpdateTime
                                 & surfaces.fontSurface .~ fontSurface'

            return newState

        Nothing -> return $ state & fpsState.oldTime .~ newTime

updateScreen :: Game -> IO Game
updateScreen state = do
    newTime <- getCurrentTime
    if addUTCTime (state^.world^.refreshRate)
            (state^.lastScreenUpdateTime) >= newTime
        then return state
        else do
            writeToScreen state bgSurface 
            writeToScreenWithPos state boxSurface
                (state^.positions.boxPosition)
            writeToScreenWithPos state ballSurface
                (state^.positions.ballPosition)
            writeToScreen state fontSurface
            updateWindowSurface (state^.world.mainWindow)
            newState <- updateFPSCounter state
            return $ newState & lastScreenUpdateTime .~ newTime

writeToScreen :: Game -> ((Surface -> Const Surface Surface) -> Surfaces
                      -> Const Surface Surfaces) -> IO ()
writeToScreen s source = surfaceBlit (s^.surfaces.source) Nothing
                         (s^.surfaces.screenSurface) Nothing

--have type signatures gone too far?
writeToScreenWithPos :: Game -> ((Surface -> Const Surface Surface)
                             -> Surfaces -> Const Surface Surfaces)
                             -> Point V2 CInt -> IO ()
writeToScreenWithPos s source pos = surfaceBlit (s^.surfaces.source) Nothing
                                    (s^.surfaces.screenSurface) (Just pos)

updatePositions :: Game -> IO Game
updatePositions state = do
    newTime <- getCurrentTime
    if addUTCTime movementDelay (state^.lastUpdateTime) >= newTime
        then return state
        else do
            events <- pollEvents
            if windowClosed events
                then return $ state & exit .~ True
                else do
                    let updateTime x = x & lastUpdateTime .~ newTime
                    updateTime <$> ((`updateBox` events) =<< updateBall state)
