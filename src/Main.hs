module Main (main) where

import Drawing
import Logic
import State

import Graphics.Gloss

main :: IO ()
main =
  play
    (InWindow
      "Snake"
      (floor windowSize, floor windowSize)
      (100, 100))
    white
    3
    initialState
    makePicture
    handleEvent
    stepWorld
