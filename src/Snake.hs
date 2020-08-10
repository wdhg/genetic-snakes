module Snake where

import Control.Monad.State
import System.Random

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
    , stdGen    :: StdGen
    }

instance Show SnakeGame where
  show game@(SnakeGame snake (width, height) food _ _)
    = concatMap showRow [0..height - 1]
      where
        showRow :: Int -> String
        showRow y
          = concatMap (\x -> showCell (x,y)) [0..width - 1] ++ "\n"
        showCell :: Vector -> String
        showCell cell
          | cell `elem` snake = "# "
          | cell == food      = "@ "
          | otherwise         = ". "

s = SnakeGame [(0,0)] (5,5) (3,0) right $ mkStdGen 0

add :: Vector -> Vector -> Vector
add (x1, y1) (x2, y2)
  = (x1 + x2, y1 + y2)

extend :: Monad m => StateT SnakeGame m ()
extend
  = do
    currentGame <- get
    let currentSnake = snake currentGame
    put (currentGame
      { snake = add (direction currentGame) (head currentSnake) : currentSnake
      })

eatFood :: StateT SnakeGame IO ()
eatFood
  = do
    extend
    currentGame <- get
    let (maxX, maxY) = bounds currentGame
        cells = [(x, y) | x <- [0..maxX - 1], y <- [0..maxY - 1]]
        choices = filter (`notElem` (snake currentGame)) cells
        (index, gen') = randomR (0, length choices - 1) $ stdGen currentGame
    put (currentGame
      { food = choices !! index
      , stdGen = gen'
      })

move :: Monad m => StateT SnakeGame m ()
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

update :: StateT SnakeGame IO ()
update
  = do
    currentGame <- get
    let currentBody = tail $ snake currentGame
    case add (direction currentGame) (head $ snake currentGame) of
      newHead
        | outOfBounds (bounds currentGame) newHead
          || newHead `elem` currentBody            -> return ()
        | newHead == (food currentGame)            -> eatFood
        | otherwise                                -> move
