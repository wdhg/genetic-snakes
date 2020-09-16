module Snake where

import Control.Monad.Random

type Vector
  = (Int, Int)

data GameState
  = Won
  | Lost
  | Unfinished
    deriving (Show, Eq)

up, down, left, right :: Vector
up    = (0,-1)
down  = (0,1)
left  = (-1,0)
right = (1,0)

data Snake
  = Snake
    { body      :: [Vector]
    , direction :: Vector
    }
    deriving (Show, Eq)

data Game
  = Game
    { snake  :: Snake
    , bounds :: Vector
    , food   :: Vector
    }
    deriving (Show)

add :: Vector -> Vector -> Vector
add (x0, y0) (x1, y1)
  = (x0 + x1, y0 + y1)

extendHead :: Snake -> Snake
extendHead snake'
  = let newHead = add (head $ body snake') $ direction snake'
     in snake' {body = newHead : body snake'}

shrinkTail :: Snake -> Snake
shrinkTail snake'
  = snake' {body = init $ body snake'}

getEmptyCells :: Game -> [Vector]
getEmptyCells game
  = [(x,y)
    | x <- [0..maxX-1]
    , y <- [0..maxY-1]
    , (x,y) `notElem` (body $ snake game)]
      where
        (maxX, maxY) = bounds game

spawnFood :: RandomGen g => Game -> Rand g Game
spawnFood game
  = do
    maybeCell <- uniformMay $ getEmptyCells game
    case maybeCell of
      Nothing   -> return game
      Just cell -> return $ game {food = cell}

setDirection :: Vector -> Snake -> Snake
setDirection dir snake'
  | (add dir $ direction snake') == (0,0)
    = snake' -- can't move back on self
  | otherwise
    = snake' {direction = dir}

hasLost :: Game -> Bool
hasLost game
  = let (snakeHead@(x,y) : snakeTail) = body $ snake game
        (maxX, maxY) = bounds game
     in snakeHead `elem` snakeTail || x < 0 || y < 0 || x >= maxX || y >= maxY

getGameState :: Game -> GameState
getGameState game
  | hasLost game             = Lost
  | getEmptyCells game == [] = Won
  | otherwise                = Unfinished

update :: RandomGen g => Vector -> Game -> Rand g Game
update dir game
  | (head $ body snake') == food game
    = spawnFood game {snake = snake'}
  | otherwise
    = return game {snake = shrinkTail snake'}
      where
        snake' = extendHead $ setDirection dir $ snake game
