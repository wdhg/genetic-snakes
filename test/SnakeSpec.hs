module SnakeSpec where

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
runUpdates
  = foldl (flip update) testGame -- foldl so moves work in order provided

spec :: Spec
spec
  = describe "update" $ do
      it "should move the snake properly" $ do
        let result = snake $ runUpdates [right, down, down, left]
        result `shouldBe` (Snake [(2,2), (3,2), (3,1), (3,0), (2,0)] left)
