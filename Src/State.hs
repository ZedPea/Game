{-# LANGUAGE TemplateHaskell #-}

module State where

import SDL
import SDL.TTF.FFI
import Data.Time
import Foreign.C
import Control.Lens

data Game = Game {
    _fpsState :: FPSCounterState,
    _world :: World,
    _surfaces :: Surfaces,
    _upBlocks :: [Block],
    _downBlocks :: [Block],
    _heliPosition :: Point V2 CInt,
    _lastUpdateTime :: UTCTime,
    _lastScreenUpdateTime :: UTCTime,
    _score :: Int,
    _dead :: Bool,
    _exit :: Bool
}

data FPSCounterState = FPSCounterState {
    _oldTime :: UTCTime,
    _lastFPSUpdateTime :: UTCTime
}

data Surfaces = Surfaces {
    _screenSurface :: Surface,
    _bgSurface :: Surface,
    _boxSurface :: Surface,
    _fontSurface :: Surface,
    _blockSurface :: Surface,
    _menuBGSurface :: Surface,
    _deadBGSurface :: Surface
}

data World = World {
    _mainWindow :: Window,
    _font :: TTFFont,
    _refreshRate :: NominalDiffTime,
    _screenWidth :: CInt,
    _screenHeight :: CInt,
    _screenSize :: V2 CInt
}

data Block = Block {
    _size :: Rectangle CInt,
    _position :: Point V2 CInt
}

makeLenses ''Game
makeLenses ''FPSCounterState
makeLenses ''Surfaces
makeLenses ''World
makeLenses ''Block
