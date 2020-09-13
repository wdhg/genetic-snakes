module Snake
  ( up
  , down
  , left
  , right
  , update
  , newGame
  , Vector
  , GameState(..)
  ) where

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

data GameState
  = GameState
    { snake    :: Snake
    , snakeDir :: Vector
    , bounds   :: Vector
    , food     :: Vector
    , gen      :: StdGen
    , finished :: Bool
    }
      deriving (Show)

type Predicate
  = State GameState Bool

type Action
  = State GameState ()

printGame :: GameState -> IO ()
printGame game
  = do
    let (width, height) = bounds game
        bar = concat $ replicate width "━━"
        showCell cell
          | cell == (head $ snake game) = "██"
          | cell `elem` (snake game)    = "▓▓"
          | cell == (food game)         = "▒▒"
          | otherwise                   = "  "
        showRow y
          = "\n┃ " ++ concatMap (\x -> showCell (x,y)) [0..width - 1] ++ " ┃"
    putStr $ "\n┏━" ++ bar ++ "━┓"
    mapM_ (putStr . showRow) [0..height - 1]
    putStr $ "\n┗━" ++ bar ++ "━┛\n"

(?) :: Predicate -> Action -> Action
predicate ? action
  = do
    result <- predicate
    if result
       then action
       else state $ \s -> ((), s)

add :: Vector -> Vector -> Vector
add (x1, y1) (x2, y2)
  = (x1 + x2, y1 + y2)

notFinished :: Predicate
notFinished
  = state $ \s -> (not $ finished s, s)

outOfBounds :: Predicate
outOfBounds
  = do
    game <- get
    let (x, y) = head $ snake game
        (maxX, maxY) = bounds game
    return $ x < 0 || y < 0 || x >= maxX || y >= maxY

eatenSelf :: Predicate
eatenSelf
  = do
    game <- get
    let body = snake game
    return $ head body `elem` tail body

eatenFood :: Predicate
eatenFood
  = do
    game <- get
    return $ (head $ snake $ game) == (food game)

spawnFood :: Action
spawnFood
  = do
    game <- get
    let (maxX, maxY) = bounds game
        freeCells
          = [ (x, y)
              | x <- [0..maxX - 1]
              , y <- [0..maxY - 1]
              , (x, y) `notElem` (snake game)
            ]
        (index, gen') = randomR (0, length freeCells - 1) $ gen game
    case freeCells of
      [] -> finishGame -- no free cells left
      _  -> put (game
              { food = freeCells !! index
              , gen  = gen'
              })

validDirection :: Vector -> Predicate
validDirection direction
  = do
    game <- get
    let oldDirection = snakeDir game
    return $ (length $ snake game) == 1 || (and $ map not
      [ direction == up && oldDirection == down
      , direction == down && oldDirection == up
      , direction == left && oldDirection == right
      , direction == right && oldDirection == left
      ])

move :: Vector -> Action
move direction
  = do
    game <- get
    isValidDirection <- validDirection direction
    let currentSnake = snake game
        direction'
          | isValidDirection = direction
          | otherwise        = snakeDir game
        newHead = add direction' (head currentSnake)
    put (game
      { snake = newHead : currentSnake
      })

shrinkTail :: Action
shrinkTail
  = state $ \game -> ((), game {snake = init $ snake game})

finishGame :: Action
finishGame
  = state $ \game -> ((), game {finished = True})

update :: Vector -> Action
update direction
  = notFinished ? do
      move direction
      hasEatenFood <- eatenFood
      if hasEatenFood
         then spawnFood
         else shrinkTail
      outOfBounds ? finishGame
      eatenSelf ? finishGame

newGame :: Int -> Int -> Int -> GameState
newGame width height seed
  | width <= 1 || height <= 1
    = error "Cannot have map with dimensions <= 1"
  | otherwise
    = GameState
      { snake    = [(0,0)]
      , snakeDir = right
      , bounds   = (width, height)
      , food     = (width `div` 2, height `div` 2)
      , gen      = mkStdGen seed
      , finished = False
      }
