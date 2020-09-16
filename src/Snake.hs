module Snake where

type Vector
  = (Int, Int)

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

move :: Snake -> Snake
move snake'
  = let newHead = add (head $ body snake') $ direction snake'
     in snake' {body = newHead : (init $ body snake')}

setDirection :: Vector -> Snake -> Snake
setDirection dir snake'
  | (add dir $ direction snake') == (0,0)
    = snake' -- can't move back on self
  | otherwise
    = snake' {direction = dir}

update :: Vector -> Game -> Game
update dir game
  = game {snake = move $ setDirection dir $ snake game}
