{-# LANGUAGE TemplateHaskell #-}

module State where

import SDL (Surface(..), Window)
import SDL.TTF.FFI (TTFFont)

import Data.Time (UTCTime)
import Control.Lens (makeLenses)

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
}

data Frames = Frames {
    _fps :: Int,
    _latestFrameTime :: UTCTime,
    _previousFrameTime :: UTCTime
}

data Surfaces = Surfaces {
    _screenSurface :: Surface,
    _heliSurface :: Surface
}

data World = World {
    _mainWindow :: Window,
    _font :: TTFFont,
    _screenWidth :: Double,
    _screenHeight :: Double
}

data Blocks = Blocks {
    _blockPairs :: [(Block, Block)],
    _trend :: Direction
}

data Block = Block {
    _bottomLeft :: Coord,
    _bottomRight :: Coord,
    _topLeft :: Coord,
    _topRight :: Coord
} deriving (Show)

data Coord = Coord {
    _x :: Double,
    _y :: Double
} deriving (Show)

data Direction = Up | Down

makeLenses ''Game
makeLenses ''Frames
makeLenses ''Surfaces
makeLenses ''World
makeLenses ''Block
makeLenses ''Blocks
makeLenses ''Coord
