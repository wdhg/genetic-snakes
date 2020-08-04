module Snake where

type Vector
  = (Int, Int)

type Snake
  = [Vector]

data SnakeGame
  = SnakeGame
    { snake  :: Snake
    , bounds :: Vector
    , food   :: Vector
    }
