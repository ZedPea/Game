module Constant where

import SDL
import Foreign.C
import Data.Time
import qualified SDL.Raw as Raw

blockWidth :: CInt
blockWidth = 40

heliMovementMultiplier :: CInt
heliMovementMultiplier = 20

upKeys :: [Scancode]
upKeys = [ScancodeW]

startKeys :: [Scancode]
startKeys = [ScancodeReturn]

quitKeys :: [Scancode]
quitKeys = [ScancodeQ]

--seconds between the fps counter updates
fpsCounterUpdateDelay :: NominalDiffTime
fpsCounterUpdateDelay = 200

--seconds between game logic updates, i.e. moving objects etc
movementDelay :: NominalDiffTime
movementDelay = 0.01

titaniumWhite :: Raw.Color
titaniumWhite = Raw.Color 255 255 255 0

fontSize :: Int
fontSize = 32

bgLocation :: String
bgLocation = "Assets/background.jpg"

heliLocation :: String
heliLocation = "Assets/heli.png"

blockLocation :: String
blockLocation = "Assets/block.jpg"

menuBGLocation :: String
menuBGLocation = "Assets/menuBG.jpg"

deadBGLocation :: String
deadBGLocation = "Assets/deadBG.jpg"

fontLocation :: String
fontLocation = "Fonts/arial.ttf"

deadMessage :: String
deadMessage = "You died! Hit enter to play again, or q to quit."

menuMessage :: String
menuMessage = "Hit enter to start the game! Use W to fly!"

charLength :: CInt
charLength = 13

charHeight :: CInt
charHeight = 20

scoreWhiteSpace :: CInt
scoreWhiteSpace = 18

heliHeight :: CInt
heliHeight = 63

heliLength :: CInt
heliLength = 104

maxIncrement :: (Num a) => a
maxIncrement = 150
