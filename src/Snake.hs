module Snake
  ( up
  , down
  , left
  , right
  , update
  , newGame
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

data SnakeGame
  = SnakeGame
    { snake    :: Snake
    , bounds   :: Vector
    , food     :: Vector
    , gen      :: StdGen
    , finished :: Bool
    }
      deriving (Show)

type Predicate
  = StateT SnakeGame IO Bool

type Action
  = StateT SnakeGame IO ()

printGame :: SnakeGame -> IO ()
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
    currentGame <- get
    let (x, y) = head $ snake currentGame
        (maxX, maxY) = bounds currentGame
    return $ x < 0 || y < 0 || x >= maxX || y >= maxY

eatenSelf :: Predicate
eatenSelf
  = do
    currentGame <- get
    let body = snake currentGame
    return $ head body `elem` tail body

eatenFood :: Predicate
eatenFood
  = do
    currentGame <- get
    return $ (head $ snake $ currentGame) == (food currentGame)

spawnFood :: Action
spawnFood
  = do
    currentGame <- get
    let (maxX, maxY) = bounds currentGame
        freeCells
          = [ (x, y)
              | x <- [0..maxX - 1]
              , y <- [0..maxY - 1]
              , (x, y) `notElem` (snake currentGame)
            ]
        (index, gen') = randomR (0, length freeCells - 1) $ gen currentGame
    case freeCells of
      [] -> finishGame -- no free cells left
      _  -> put (currentGame
              { food = freeCells !! index
              , gen  = gen'
              })

extend :: Vector -> Action
extend direction
  = do
    currentGame <- get
    let currentSnake = snake currentGame
        newHead = add direction (head currentSnake)
    put (currentGame
      { snake = newHead : currentSnake
      })

shrinkTail :: Action
shrinkTail
  = state $ \currentGame -> ((), currentGame {snake = init $ snake currentGame})

finishGame :: Action
finishGame
  = state $ \currentGame -> ((), currentGame {finished = True})

update :: Vector -> Action
update direction
  = notFinished ? do
      extend direction
      hasEatenFood <- eatenFood
      if hasEatenFood
         then spawnFood
         else shrinkTail
      outOfBounds ? finishGame
      eatenSelf ? finishGame

newGame :: Int -> Int -> Int -> SnakeGame
newGame width height seed
  | width <= 1 || height <= 1
    = error "Cannot have map with dimensions <= 1"
  | otherwise
    = SnakeGame
      { snake    = [(0,0)]
      , bounds   = (width, height)
      , food     = (width `div` 2, height `div` 2)
      , gen      = mkStdGen seed
      , finished = False
      }
