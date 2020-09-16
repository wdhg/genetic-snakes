module SnakeSpec where

import Control.Monad.Loops  (concatM)
import Control.Monad.Random
import Snake
import Test.Hspec

testGame :: Game
testGame
  = Game
    { snake  = Snake [(2,0), (1,0), (0,0), (0,1), (0,2)] right
    , bounds = (4,4)
    , food   = (3,1)
    }

runUpdates :: [Vector] -> Game
runUpdates moves
  = evalRand (updates testGame) (mkStdGen 0)
    where
      updates = concatM $ map update moves

spec :: Spec
spec
  = describe "Snake" $ do
      describe "update" $ do
        it "should move the snake properly" $ do
          let result = snake $ runUpdates [down, down, left]
          result `shouldBe` (Snake [(1,2), (2,2), (2,1), (2,0), (1,0)] left)
        it "should increase the length of the snake upon eating the food" $ do
          let result = snake $ runUpdates [right, down]
          result `shouldBe` (Snake [(3,1), (3,0), (2,0), (1,0), (0,0), (0,1)] down)

      describe "getGameState" $ do
        it "should return Lost if snake out of bounds" $ do
          let result = getGameState $ Game (Snake [(0,3),(0,2)] down) (3,3) (2,2)
          result `shouldBe` Lost
        it "should return Won if snake fills grid" $ do
          let result = getGameState $ Game (Snake [(0,0),(1,0),(1,1),(0,1)] left) (2,2) (2,2)
          result `shouldBe` Won
        it "should return Unfinished if snake in bounds" $ do
          let result = getGameState $ Game (Snake [(2,2),(2,1)] down) (3,3) (2,2)
          result `shouldBe` Unfinished
