module Screen where

import SDL
import SDL.TTF
import Foreign.C
import Control.Lens
import Data.List
import Data.Time
import State
import Constant
import Update

writeStaticScreen :: Game -> ((Surface -> Const Surface Surface) -> Surfaces
                          -> Const Surface Surfaces) -> String -> IO ()
writeStaticScreen state source message = do
    writeToScreen state source
    fontSurface' <- renderUTF8Solid (state^.world.font) message titaniumWhite
    surfaceBlit fontSurface' Nothing (state^.surfaces.screenSurface) (Just pos)
    updateWindowSurface (state^.world.mainWindow)
    where pos = P $ V2 xOffset yOffset
          msgLength = charLength * genericLength message 
          xOffset = ((state^.world.screenWidth) `div` 2) - (msgLength `div` 2)
          yOffset = ((state^.world.screenHeight) `div` 2) - charHeight

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
          lengthDiff = scoreWhiteSpace * fromIntegral
                                         (length . show $ state^.score)
          startPos = (state^.world.screenWidth)
                     - (scoreWhiteSpace * fromIntegral
                       (length ("Score:" :: String)))

writeBlocks :: Game -> IO ()
writeBlocks state = do
    mapM_ (\x -> writeToScreenBothPos state blockSurface
          (x^.size) (x^.position)) (state^.upBlocks)
    mapM_ (\x -> writeToScreenBothPos state blockSurface
          (x^.size) (x^.position)) (state^.downBlocks)
