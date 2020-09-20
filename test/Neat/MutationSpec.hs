module Neat.MutationSpec where

import Control.Monad.Random
import Control.Monad.State
import Neat
import Test.Hspec

g0, g1, g2 :: Gene
g0 = Gene (Link (Id 0) (Id 1)) 4.0 True (Id 0)
g1 = Gene (Link (Id 0) (Id 2)) 1.0 True (Id 1)
g2 = Gene (Link (Id 2) (Id 1)) 4.0 True (Id 2)

genome0, genome1 :: Genome
genome0
  = Genome [g2, g1, g0 {enabled = False}] [Id 0] [Id 1] [Id 2]
genome1
  = Genome [g0] [Id 0] [Id 1] []
genome2
  = Genome [] [Id 0] [] [Id 1]

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
