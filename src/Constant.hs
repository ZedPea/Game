module Constant
(
    titaniumWhite,
    charLength,
    charHeight,
    scoreWhiteSpace,
    heliWidth,
    heliHeight,
    blockWidth,
    upKeys,
    heliMovementMultiplier,
    heliLocation,
    fontLocation,
    fontSize,
    menuMessage,
    deadMessage,
    fpsCounterUpdateDelay,
    tickRate,
    startKeys,
    quitKeys,
    blockIncrement,
    blockGap
)
where

import SDL
import SDL.Raw (Color(..))

--microseconds between game updates (not screen refreshes)
tickRate :: Int
tickRate = round $ oneSecond / 128

blockWidth :: Double
blockWidth = 40

heliMovementMultiplier :: Double
heliMovementMultiplier = 4

upKeys :: [Scancode]
upKeys = [ScancodeW]

startKeys :: [Scancode]
startKeys = [ScancodeReturn]

quitKeys :: [Scancode]
quitKeys = [ScancodeQ]

--microseconds between the fps counter updates
fpsCounterUpdateDelay :: Int
fpsCounterUpdateDelay = round $ 0.2 * oneSecond

oneSecond :: Double
oneSecond = 10 ^ 6

titaniumWhite :: Color
titaniumWhite = Color 255 255 255 0

fontSize :: Int
fontSize = 32

heliLocation :: String
heliLocation = "assets/heli.png"

fontLocation :: String
fontLocation = "fonts/arial.ttf"

deadMessage :: String
deadMessage = "You died! Hit enter to play again, or q to quit."

menuMessage :: String
menuMessage = "Hit enter to start the game. Hold W to go up."

charLength :: Double
charLength = 13

charHeight :: Double
charHeight = 20

scoreWhiteSpace :: Double
scoreWhiteSpace = 18

heliHeight :: Double
heliHeight = 40

heliWidth :: Double
heliWidth = 105

blockIncrement :: Double
blockIncrement = 20

blockGap :: Double
blockGap = 300
