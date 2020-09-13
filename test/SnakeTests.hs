module SnakeTests (tests) where

import Control.Monad.State
import Snake
import System.Random
import Test.HUnit
import Utils

runGameCustom :: GameState -> [Vector] -> GameState
runGameCustom game inputs
  = snd $ runState (mapM_ update inputs) game

runGame :: [Vector] -> GameState
runGame
  = runGameCustom $ newGame 6 6 0

tests :: Test
tests
  = TestList
    [ testEqual "Inputs move snake" [(2, 3)] $
        snake $ runGame [right, right, down, down, down]
    , testBool "Moving out of top bound ends game"  $
        finished $ runGame [up]
    , testBool "Moving out of bottom bound ends game"  $
        finished $ runGame [down, down, down, down, down, down]
    , testBool "Moving out of left bound ends game"  $
        finished $ runGame [left]
    , testBool "Moving out of right bound ends game"  $
        finished $ runGame [right, right, right, right, right, right]
    , testBool "Moving in bounds doesn't end game" $
        not $ finished $ runGame
          [right, right, right, right, right, down, down, down, down, down]
    , testEqual "Eating food extends snake" 2 $
        length $ snake $ runGame [right, right, right, down, down, down]
    , testBool "Eating self ends game" $
        finished $ runGameCustom
          (GameState [(0,0),(1,0),(1,1),(0,1),(0,2)] left (4,4) (3,3) (mkStdGen 0) False)
          [down]
    , testBool "Filling board ends game" $
        finished $ runGameCustom
          (GameState [(0,0),(1,0),(1,1)] (2,2) (0,1) left (mkStdGen 0) False)
          [down]
    , testEqual "Snake is unable to back on itself" [(2,0),(1,0)] $
        snake $ runGameCustom
          (GameState [(1,0),(0,0)] right (4,4) (3,3) (mkStdGen 0) False)
          [left]
    ]
