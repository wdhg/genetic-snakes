{-# LANGUAGE TupleSections #-}

module Snake where

import Control.Monad.State

type Vector
  = (Int, Int)

up, down, left, right :: Vector
up    = (0, 1)
down  = (0, -1)
left  = (-1, 0)
right = (1, 0)

type Snake
  = [Vector]

data SnakeGame
  = SnakeGame
    { snake     :: Snake
    , bounds    :: Vector
    , food      :: Vector
    , direction :: Vector
    }

instance Show SnakeGame where
  show game@(SnakeGame snake (width, height) food _)
    = concatMap showRow [0..height]
      where
        showRow :: Int -> String
        showRow y
          = concatMap (showCell . (,y)) [0..width] ++ "\n"
        showCell :: Vector -> String
        showCell cell
          | cell `elem` snake = "# "
          | cell == food      = "@ "
          | otherwise         = ". "

add :: Vector -> Vector -> Vector
add (x1, y1) (x2, y2)
  = (x1 + x2, y1 + y2)

extend :: State SnakeGame ()
extend
  = do
    currentGame <- get
    let currentSnake = snake currentGame
    put (currentGame
      { snake = add (direction currentGame) (head currentSnake) : currentSnake
      })

move :: State SnakeGame ()
move
  = do
    currentGame <- get
    let currentSnake = snake currentGame
    put (currentGame
      { snake = add (direction currentGame) (head currentSnake) : (init currentSnake)
      })

outOfBounds :: Vector -> Vector -> Bool
outOfBounds (maxX, maxY) (x, y)
  = x < 0 || y < 0 || x >= maxX || y >= maxY

update :: State SnakeGame ()
update
  = do
    currentGame <- get
    case add (direction currentGame) (head $ snake currentGame) of
      newHead
        | outOfBounds (bounds currentGame) newHead -> return ()
        | newHead == (food currentGame)            -> extend
        | otherwise                                -> move
