module Init
(
    initialState,
    reInit,
)
where

import Control.Lens ((^.), (&), (.~))
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_)
import Data.Time (getCurrentTime)

import State (Surfaces, World, Game(..), Frames(..), Blocks(..), Block(..), 
              Coord(..), Direction(..), world, heliPosition, score, dead, exit,
              blocks, blockPairs, screenWidth, screenHeight, speed, upPressed)

import Constant (heliWidth, heliHeight)

initialState :: Surfaces -> World -> IO (MVar Game)
initialState surfaces' world' = do
    time1 <- getCurrentTime
    time2 <- getCurrentTime 

    let frames' = Frames 0 time1 time2

    newMVar $
        Game frames' world' surfaces' blocks' heliPosition' 1.0 0 False 
             False False
    where heliPosition' = heliPosInit world'
          blocks' = Blocks [] Up

reInit :: MVar Game -> IO ()
reInit stateMVar = modifyMVar_ stateMVar $ \state -> return $ 
    state & blocks.blockPairs .~ []
          & heliPosition .~ heliPosInit (state^.world)
          & score .~ 0
          & speed .~ 1.0
          & dead .~ False
          & exit .~ False
          & upPressed .~ False

heliPosInit :: World -> Block
heliPosInit world' = Block (Coord (midX - halfHeliWidth)
                                  (midY + halfHeliHeight))

                           (Coord (midX + halfHeliWidth)
                                  (midY + halfHeliHeight))

                           (Coord (midX - halfHeliWidth)
                                  (midY - halfHeliHeight))

                           (Coord (midX + halfHeliWidth) 
                                  (midY - halfHeliHeight))

    where midX = (world'^.screenWidth) / 2
          midY = (world'^.screenHeight) / 2
          halfHeliWidth = heliWidth / 2
          halfHeliHeight = heliHeight / 2
