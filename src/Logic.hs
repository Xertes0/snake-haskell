module Logic where

import State

import System.Random
import Graphics.Gloss.Interface.Pure.Game

handleEvent :: Event -> State -> State
handleEvent event state
  | EventKey (Char 'w') Down _ _ <- event
  = state { direction = (0,1) }
  | EventKey (Char 's') Down _ _ <- event
  = state { direction = (0,-1) }
  | EventKey (Char 'd') Down _ _ <- event
  = state { direction = (1,0) }
  | EventKey (Char 'a') Down _ _ <- event
  = state { direction = (-1,0) }
  | otherwise = state

moveCell :: (Int,Int) -> (Int,Int) -> (Int,Int)
moveCell (x,y) (a,b) = (x+a,y+b)

foldRandomGen :: ([Int], StdGen) -> (StdGen -> (Int, StdGen)) -> ([Int], StdGen)
foldRandomGen (arr, gen) func = (arr ++ [newNum], newGen)
  where
    (newNum, newGen) = (func gen)

randomFood :: State -> ((Int, Int), StdGen)
randomFood state = ((x,y), newGen)
  where
    ([x,y], newGen) = foldl foldRandomGen ([], randomGen state) [ uniformR (0, cellCount-1) | _ <- [1..2] ]

checkFood :: State -> State
checkFood state = case ate of
  True -> state { food = fst newFood
                , randomGen = snd newFood
                , stall = True }
  False -> state
  where
    ate = (food state) == (last $ body state)
    newFood = randomFood state

noDuplicates :: (Eq a) => [a] -> Bool
noDuplicates [] = True
noDuplicates (x:xs) = x `notElem` xs && noDuplicates xs

checkGameOver :: State -> State
checkGameOver state = state { gameState = newState }
  where
    hitWallFilter = (\(x, y) ->
                       x /= (-1) && y /= (-1) && x /= cellCount && y /= cellCount)
    hitWall = 0 == ( length $ filter hitWallFilter [last $ body state] )
    hitSelf = not $ noDuplicates $ body state
    newState = case hitWall || hitSelf of
      True -> GameOver
      False -> gameState state

stepWorld :: Float -> State -> State
stepWorld _ state = case gameState state of
  Playing -> checkGameOver $ checkFood $ state { body = newBody, stall = False }
  GameOver -> state
  where
    oldBody = case stall state of
      True -> body state
      False -> (tail $ body state)
    newBody = oldBody ++ [moveCell (last $ body state) (direction state)]
