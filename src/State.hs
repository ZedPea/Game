{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module State where

import SDL (Surface(..), Window)
import SDL.TTF.FFI (TTFFont)

import Data.Time (UTCTime)
import Control.Lens (makeLenses)
import GHC.Generics
import Control.DeepSeq (NFData, rnf)

data Game = Game {
    _frames :: Frames,
    _world :: World,
    _surfaces :: Surfaces,
    _blocks :: Blocks,
    _heliPosition :: Block,
    _speed :: Double,
    _score :: Int,
    _upPressed :: Bool,
    _dead :: Bool,
    _exit :: Bool
} deriving (Generic, NFData)

data Frames = Frames {
    _fps :: Int,
    _latestFrameTime :: UTCTime,
    _previousFrameTime :: UTCTime
} deriving (Generic, NFData)

data Surfaces = Surfaces {
    _screenSurface :: Surface,
    _heliSurface :: Surface
} deriving (Generic, NFData)

instance NFData Surface where rnf x = seq x ()

data World = World {
    _mainWindow :: Window,
    _font :: TTFFont,
    _screenWidth :: Double,
    _screenHeight :: Double
} deriving (Generic, NFData)

instance NFData Window where rnf x = seq x ()

data Blocks = Blocks {
    _blockPairs :: [(Block, Block)],
    _trend :: Direction
} deriving (Generic, NFData)

data Block = Block {
    _bottomLeft :: Coord,
    _bottomRight :: Coord,
    _topLeft :: Coord,
    _topRight :: Coord
} deriving (Generic, NFData)

data Coord = Coord {
    _x :: Double,
    _y :: Double
} deriving (Generic, NFData)

data Direction = Up | Down deriving (Generic, NFData)

makeLenses ''Game
makeLenses ''Frames
makeLenses ''Surfaces
makeLenses ''World
makeLenses ''Block
makeLenses ''Blocks
makeLenses ''Coord
