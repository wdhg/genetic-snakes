module Neat.MutationSpec where

import Control.Monad.Random
import Control.Monad.State
import Neat
import Neat.TestUtils
import Test.Hspec

spec :: Spec
spec
  = do
    describe "mutateWeights" $ do
      it "should adjust weights of genes in genome" $ do
        let result = evalRand (mutateWeights genome0) (mkStdGen 0)
        map weight (genes result) `shouldNotBe` map weight (genes genome0)

    describe "mutateNode" $ do
      it "should replace a gene with two new genes" $ do
        let (resultGenome, resultInnovations)
              = evalRand (mutateNode genome1 [(Link (Id 0) (Id 1), Id 0)]) (mkStdGen 0)
        resultGenome `shouldBe` genome0

    describe "mutateLink" $ do
      it "should create a new link random between unlinked nodes" $ do
        let (resultGenome, resultInnovations)
              = evalRand (mutateLink genome2 [(Link (Id 2) (Id 1), Id 0) ]) (mkStdGen 0)
        (length $ genes resultGenome) `shouldBe` 1
        (innovationID $ head $ genes resultGenome) `shouldBe` (Id 1)
        (link $ head $ genes resultGenome) `shouldBe` (Link (Id 0) (Id 1))
