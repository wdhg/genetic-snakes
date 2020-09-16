module NeatSpec where

import Control.Monad.Random
import Neat
import Test.Hspec

g0 :: Gene
g0 = Gene (Input 0) (Output 0) 0.0 True 0

spec :: Spec
spec
  = describe "Neat" $ do
      describe "perturbWeight" $ do
        it "should adjust weights of genes" $ do
          weight (evalRand (perturbWeight g0) (mkStdGen 0)) `shouldNotBe` 0.0
