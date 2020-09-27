module Neat.PopulationSpec where

import Control.Monad.Random
import Neat
import Neat.TestUtils
import Test.Hspec

spec :: Spec
spec
  = do
    describe "newPopulation" $ do
      let result = evalRand (newPopulation 25 genome2) (mkStdGen 0)
          resultOrganisms= concatMap members $ species result
      it "produces a population of a desired size" $ do
        length resultOrganisms `shouldBe` 25
      it "each offspring should have one more gene than the template" $ do
        mapM_ (\g -> (length $ genes $ genome g) `shouldBe` 1) resultOrganisms
