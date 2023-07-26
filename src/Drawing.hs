module Drawing where

import State

import Graphics.Gloss

windowSize :: Float
windowSize = 800

cellSize :: Float
cellSize = windowSize / fromIntegral cellCount

drawCell :: (Int, Int) -> Picture
drawCell (x, y) =
  translate
  (fromIntegral x * cellSize + cellSize / 2.0)
  (fromIntegral y * cellSize + cellSize / 2.0)
  $ rectangleSolid cellSize cellSize

translateToScreenCoords :: Picture -> Picture
translateToScreenCoords = translate ((-windowSize) / 2.0) ((-windowSize) / 2.0)

makePicture :: State -> Picture
makePicture state =
  pictures
  $ map translateToScreenCoords
  $ [snakePicture, foodPicture]
  where
    snakePicture = pictures $ map ((color black) . drawCell) $ body state
    foodPicture = color green $ drawCell $ food state
