module Utilities
(
    calculateFPS,
    keysPressed,
    windowClosed,
    blockToRectangle,
    blockToPoint,
    listOfPairsToList,
    handleUpPress
)
where


import SDL (Event, EventPayload(..), Scancode, V2(..), Point(..),
            InputMotion(..), Rectangle(..), eventPayload, keysymScancode,
            keyboardEventKeyMotion, keyboardEventKeysym)

import Data.Time (UTCTime, diffUTCTime)
import Foreign.C (CInt)
import Control.Lens ((^.), (&), (.~))
import Control.Concurrent.MVar (MVar, readMVar, modifyMVar_)
import Control.Monad (when)

import Constant (upKeys)
import State (Game, Block, bottomLeft, topLeft, topRight, upPressed)
import qualified State as S (x, y)

windowClosed :: [Event] -> Bool
windowClosed [] = False
windowClosed (x:xs) = case eventPayload x of
    WindowClosedEvent _ -> True
    _ -> windowClosed xs

keysPressed :: Event -> [Scancode] -> Bool
keysPressed event keys = keysAltered event keys Pressed

keysReleased :: Event -> [Scancode] -> Bool
keysReleased event keys = keysAltered event keys Released

keysAltered :: Foldable t => Event -> t Scancode -> InputMotion -> Bool
keysAltered event keys someState = case eventPayload event of
    KeyboardEvent keyboardEvent ->
        keyboardEventKeyMotion keyboardEvent == someState &&
        keysymScancode (keyboardEventKeysym keyboardEvent) `elem` keys
    _ -> False

calculateFPS :: UTCTime -> UTCTime -> Int
calculateFPS frame1 frame2
    | diffUTCTime frame2 frame1 == 0 = 0 --div by 0
    | otherwise = round $ 1 / frameTime
    where frameTime = realToFrac $ diffUTCTime frame1 frame2 :: Double

blockToRectangle :: Block -> Rectangle CInt
blockToRectangle block' = makeRectangle xOrigin yOrigin width height
    where height = round $ (block'^.bottomLeft.S.y) - (block'^.topLeft.S.y)
          width = round $ (block'^.topRight.S.x) - (block'^.topLeft.S.x)

          xOrigin = round $ block'^.topLeft.S.x
          yOrigin = round $ block'^.topLeft.S.y

          makeRectangle x' y' w' h' = Rectangle (P $ V2 x' y') (V2 w' h')

blockToPoint :: Block -> Point V2 CInt
blockToPoint block' = P $ V2 (round $ block'^.topLeft.S.x)
                             (round $ block'^.topLeft.S.y)

listOfPairsToList :: [(a, a)] -> [a]
listOfPairsToList xs = map fst xs ++ map snd xs

handleUpPress :: MVar Game -> [Event] -> IO ()
handleUpPress stateMVar events = do
    state <- readMVar stateMVar
    let shouldFlip = if state^.upPressed
                        then any (`keysReleased` upKeys) events
                        else any (`keysPressed` upKeys) events

    when shouldFlip . modifyMVar_ stateMVar $ (return . flipKeyState)

    where flipKeyState s = s & upPressed .~ not (s^.upPressed)
