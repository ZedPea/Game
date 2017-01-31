module Update where

import Utilities
import SDL
import SDL.TTF
import Control.Lens
import Foreign.C
import Data.Time

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

updateScreen :: Game -> IO Game
updateScreen state = do
    newTime <- getCurrentTime
    if addUTCTime (state^.world^.refreshRate)
            (state^.lastScreenUpdateTime) >= newTime
        then return state
        else do
            writeToScreen state bgSurface 
            writeToScreenWithPos state boxSurface (state^.heliPosition)
            writeBlocks state
            writeScore state
            writeToScreen state fontSurface

            updateWindowSurface (state^.world.mainWindow)
            newState <- updateScore <$> updateFPSCounter state
            return $ newState & lastScreenUpdateTime .~ newTime

writeToScreen :: Game -> ((Surface -> Const Surface Surface) -> Surfaces
                      -> Const Surface Surfaces) -> IO ()
writeToScreen s source = surfaceBlit (s^.surfaces.source) Nothing
                         (s^.surfaces.screenSurface) Nothing

writeToScreenWithPos :: Game -> ((Surface -> Const Surface Surface)
                             -> Surfaces -> Const Surface Surfaces)
                             -> Point V2 CInt -> IO ()
writeToScreenWithPos s source pos = surfaceBlit (s^.surfaces.source) Nothing
                                    (s^.surfaces.screenSurface) (Just pos)

--have type signatures gone too far?
writeToScreenBothPos :: Game -> ((Surface -> Const Surface Surface)
                             -> Surfaces -> Const Surface Surfaces)
                             -> Rectangle CInt -> Point V2 CInt -> IO ()
writeToScreenBothPos s source pos1 pos2 = surfaceBlit (s^.surfaces.source)
                                        (Just pos1)
                                        (s^.surfaces.screenSurface)
                                        (Just pos2)

writeScore :: Game -> IO ()
writeScore state = do
    fontSurface' <- renderUTF8Solid (state^.world.font)
                    ("Score: " ++ show (state^.score)) titaniumWhite
    surfaceBlit fontSurface' Nothing (state^.surfaces.screenSurface) (Just pos)
    where pos = P $ V2 (690 - lengthDiff) 0
          lengthDiff = 18 * fromIntegral (length . show $ state^.score)

writeBlocks :: Game -> IO ()
writeBlocks state = do
    mapM_ (\x -> writeToScreenBothPos state blockSurface
          (x^.size) (x^.position)) (state^.upBlocks)
    mapM_ (\x -> writeToScreenBothPos state blockSurface
          (x^.size) (x^.position)) (state^.downBlocks)

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
                else return . updateTime . updateBlocks $ updateBox state events

updateBlocks :: Game -> Game
updateBlocks state = state & (upBlocks.traversed.position) %~
                                (\x -> updatePos x (-1) 0)
                           & (downBlocks.traversed.position) %~
                                (\x -> updatePos x (-1) 0)
