{-# LANGUAGE TupleSections #-}

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

instance Show SnakeGame where
  show game@(SnakeGame snake (width, height) food)
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
