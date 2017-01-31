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
    where pos = P $ V2 (startPos - lengthDiff) 0
          --as the score grows in digits, draw it so the score doesn't clip
          --of the edge
          lengthDiff = charLength * fromIntegral (length . show $ state^.score)
          startPos = (state^.world.screenWidth) - 110
          --amount of pixels one character in the score counter takes
          charLength = 18

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
                else updateBlocks . updateTime $ updateBox state events

updateBlocks :: Game -> IO Game
updateBlocks state = do
    (newUp, newDown) <- addAndRemoveBlocks state (newState^.upBlocks) (newState^.downBlocks)
    return $ newState & upBlocks .~ newUp
             & downBlocks .~ newDown
    where newState = state & (upBlocks.traversed.position) %~
                                (\x -> updatePos x (-1) 0)
                           & (downBlocks.traversed.position) %~
                                (\x -> updatePos x (-1) 0)

addAndRemoveBlocks :: Game -> [Block] -> [Block] -> IO ([Block], [Block])
addAndRemoveBlocks state up down
    | x <= -blockWidth = addBlocks state (tail up) (tail down)
    | otherwise = return (up, down)
    where firstUp = head up
          (P (V2 x _)) = firstUp^.position

addBlocks :: Game -> [Block] -> [Block] -> IO ([Block], [Block])
addBlocks state up down = do
    height <- randomBlockHeight
    height2 <- randomBlockHeight
    let newUp = makeBlock (state^.world) height x' True
    let newDown = makeBlock (state^.world) height2 x' False
    return (up ++ [newUp], down ++ [newDown])
    where (P (V2 x _)) = last up^.position
          x' = x + blockWidth
