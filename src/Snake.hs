module Snake where

type Vector
  = (Int, Int)

up, down, left, right :: Vector
up    = (0, -1)
down  = (0, 1)
left  = (-1, 0)
right = (1, 0)

data Snake
  = Snake
    { body      :: [Vector]
    , direction :: Vector
    }
    deriving (Show)

data SnakeGame
  = SnakeGame
    { snake  :: Snake
    , bounds :: Vector
    , food   :: Vector
    }
    deriving (Show)
