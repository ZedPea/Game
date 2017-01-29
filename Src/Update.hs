module Update where

import Utilities
import SDL
import SDL.TTF
import Control.Lens
import Foreign.C
import Data.Time

updateBoxPos :: EventPayload -> Point V2 CInt -> Point V2 CInt
updateBoxPos (KeyboardEvent data') oldPos = case key of
    ScancodeW -> updatePos oldPos 0 ((-1) * boxMovementMultiplier)
    ScancodeS -> updatePos oldPos 0 (1 * boxMovementMultiplier)
    ScancodeD -> updatePos oldPos (1 * boxMovementMultiplier) 0
    ScancodeA -> updatePos oldPos ((-1) * boxMovementMultiplier) 0
    _ -> error "Internal error - updateBoxPos ScanCode pattern not matched"
    where key = keysymScancode $ keyboardEventKeysym data'
updateBoxPos _ _ = error "Internal error - updateBoxPos KeyboardEvent pattern\
                         \ not matched"

moveBox :: [Event] -> Point V2 CInt -> Point V2 CInt
moveBox events oldPos = let movement = filter wasdPressed events
                        in  moveBox' (map eventPayload movement) oldPos
    where moveBox' [] finalPos = finalPos
          moveBox' (x:xs) oldPos' = moveBox' xs (updateBoxPos x oldPos')

updatePos :: (Num a) => Point V2 a -> a -> a -> Point V2 a
updatePos (P (V2 x y)) x' y' = P $ V2 (x + x') (y + y')

updateBox :: Game -> [Event] -> IO Game
updateBox state events
    | any wasdPressed events = do
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
