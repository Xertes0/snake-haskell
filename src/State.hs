module State where

import System.Random

data GameState
  = Playing
  | GameOver
  deriving (Show)

data State = State
  { randomGen :: StdGen
  , gameState :: GameState
  , body :: [(Int, Int)]
  , direction :: (Int, Int)
  , food :: (Int, Int)
  , stall :: Bool
  } deriving (Show)

cellCount :: Int
cellCount = 25

initialState :: State
initialState =
  State
    { randomGen = mkStdGen 123
    , gameState = Playing
    , body = [(10, 10), (10, 11)]
    , direction = (0, 1)
    , food = (2, 5)
    , stall = False
    }
