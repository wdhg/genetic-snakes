module Snake where

import Control.Monad.State
import System.Random

type Vector
  = (Int, Int)

up, down, left, right :: Vector
up    = (0, -1)
down  = (0, 1)
left  = (-1, 0)
right = (1, 0)

type Snake
  = [Vector]

data SnakeGame
  = SnakeGame
    { snake  :: Snake
    , bounds :: Vector
    , food   :: Vector
    , stdGen :: StdGen
    }

instance Show SnakeGame where
  show game@(SnakeGame snake (width, height) food _)
    = top ++ concatMap showRow [0..height - 1] ++ bot
      where
        bar = concat $ replicate width "━━"
        top = "\n┏━" ++ bar ++ "━┓\n"
        bot = "┗━" ++ bar ++ "━┛\n"
        showRow :: Int -> String
        showRow y
          = "┃ " ++ concatMap (\x -> showCell (x,y)) [0..width - 1] ++ " ┃\n"
        showCell :: Vector -> String
        showCell cell
          | cell == head snake = "██"
          | cell `elem` snake  = "▓▓"
          | cell == food       = "▒▒"
          | otherwise          = "  "

s = SnakeGame [(0,0)] (5,5) (3,0) $ mkStdGen 0

add :: Vector -> Vector -> Vector
add (x1, y1) (x2, y2)
  = (x1 + x2, y1 + y2)

extend :: Monad m => Vector -> StateT SnakeGame m ()
extend direction
  = do
    currentGame <- get
    let currentSnake = snake currentGame
    put (currentGame
      { snake = add direction (head currentSnake) : currentSnake
      })

eatFood :: Vector -> StateT SnakeGame IO ()
eatFood direction
  = do
    extend direction
    currentGame <- get
    let (maxX, maxY) = bounds currentGame
        cells = [(x, y) | x <- [0..maxX - 1], y <- [0..maxY - 1]]
        choices = filter (`notElem` (snake currentGame)) cells
        (index, gen') = randomR (0, length choices - 1) $ stdGen currentGame
    put (currentGame
      { food = choices !! index
      , stdGen = gen'
      })

move :: Monad m => Vector -> StateT SnakeGame m ()
move direction
  = do
    currentGame <- get
    let currentSnake = snake currentGame
    put (currentGame
      { snake = add direction (head currentSnake) : (init currentSnake)
      })

outOfBounds :: Vector -> Vector -> Bool
outOfBounds (maxX, maxY) (x, y)
  = x < 0 || y < 0 || x >= maxX || y >= maxY

update :: Vector -> StateT SnakeGame IO ()
update direction
  = do
    currentGame <- get
    let currentBody = tail $ snake currentGame
    case add direction (head $ snake currentGame) of
      newHead
        | outOfBounds (bounds currentGame) newHead
          || newHead `elem` currentBody            -> return ()
        | newHead == (food currentGame)            -> eatFood direction
        | otherwise                                -> move direction

newGame :: Int -> Int -> Int -> SnakeGame
newGame width height seed
  | width > 1 && height > 1 = let gen = mkStdGen seed
                                  center = (width `div` 2, height `div` 2)
                              in SnakeGame [(0,0)] (width, height) center gen
  | otherwise = error "Cannot have map with dimensions <= 1"
