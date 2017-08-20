module Screen
(
    updateScreen,
    writeStaticScreen
)
where

import SDL (Surface, Point(..), V2(..), V4(..), surfaceBlit, surfaceFillRect,
            updateWindowSurface, surfaceFillRects)

import SDL.TTF (renderUTF8Solid)

import Foreign.C (CInt)
import Control.Lens (Const, (^.))
import Data.List (genericLength)
import Control.Concurrent.MVar (MVar, readMVar)
import Data.Vector.Storable (fromList)

import State (Game, Surfaces, world, font, surfaces, screenSurface, mainWindow,
              screenWidth, screenHeight, heliSurface, heliPosition,
              score, fps, blocks, blockPairs, frames)

import Constant (titaniumWhite, charLength, charHeight, scoreWhiteSpace)
import Utilities (blockToRectangle, blockToPoint, listOfPairsToList)

writeStaticScreen :: MVar Game -> String -> IO ()
writeStaticScreen stateMVar message = do
    state <- readMVar stateMVar

    let xOffset = ((state^.world.screenWidth) / 2) - (msgLength / 2)
        yOffset = ((state^.world.screenHeight) / 2) - charHeight
        pos = P $ V2 (round xOffset) (round yOffset)

    writeBlackScreen state

    fontSurface' <- renderUTF8Solid (state^.world.font) message titaniumWhite

    surfaceBlit fontSurface' Nothing (state^.surfaces.screenSurface) (Just pos)

    updateWindowSurface (state^.world.mainWindow)
    where msgLength = charLength * genericLength message 

updateScreen :: Game -> IO ()
updateScreen state = do
    writeBlackScreen state
    writeToScreenWithPos state heliSurface (blockToPoint $ state^.heliPosition)
    writeBlocks state
    writeScore state
    writeFPS state

    updateWindowSurface (state^.world.mainWindow)

writeBlackScreen :: Game -> IO ()
writeBlackScreen state = surfaceFillRect (state^.surfaces.screenSurface)
                         Nothing (V4 0 0 0 0)

writeToScreenWithPos :: Game -> ((Surface -> Const Surface Surface)
                             -> Surfaces -> Const Surface Surfaces)
                             -> Point V2 CInt -> IO ()
writeToScreenWithPos s source pos = surfaceBlit (s^.surfaces.source) Nothing
                                    (s^.surfaces.screenSurface) (Just pos)

writeScore :: Game -> IO ()
writeScore state = do
    fontSurface <- renderUTF8Solid (state^.world.font) msg titaniumWhite
    surfaceBlit fontSurface Nothing (state^.surfaces.screenSurface) (Just pos)
    where pos = P $ V2 (round $ startPos - lengthDiff) 0

          --as the score grows in digits, draw it so the score doesn't clip
          --of the edge

          lengthDiff = scoreWhiteSpace * (genericLength . show $ state^.score)
          startPos = (state^.world.screenWidth)
                   - (scoreWhiteSpace * genericLength "Score:")

          msg = "Score: " ++ show (state^.score)

writeBlocks :: Game -> IO ()
writeBlocks state = surfaceFillRects (state^.surfaces.screenSurface) 
                    rectangles colour

    where allBlocks = listOfPairsToList (state^.blocks.blockPairs)
          rectangles = fromList $ map blockToRectangle allBlocks
          colour = V4 192 192 192 0

writeFPS :: Game -> IO ()
writeFPS state = do
    fontSurface <- renderUTF8Solid (state^.world.font) msg titaniumWhite
    surfaceBlit fontSurface Nothing (state^.surfaces.screenSurface) Nothing
    where msg = "FPS: " ++ show (state^.frames.fps)
