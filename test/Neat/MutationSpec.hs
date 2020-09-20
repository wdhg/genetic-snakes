module Neat.MutationSpec where

import Control.Monad.Random
import Control.Monad.State
import Neat
import Test.Hspec

g0, g1, g2 :: Gene
g0 = Gene (Link (NodeID 0) (NodeID 1)) 4.0 True (InnovationID 0)
g1 = Gene (Link (NodeID 0) (NodeID 2)) 1.0 True (InnovationID 1)
g2 = Gene (Link (NodeID 2) (NodeID 1)) 4.0 True (InnovationID 2)

genome0, genome1 :: Genome
genome0
  = Genome [g2, g1, g0 {enabled = False}] [NodeID 0] [NodeID 1] [NodeID 2]
genome1
  = Genome [g0] [NodeID 0] [NodeID 1] []
genome2
  = Genome [] [NodeID 0] [] [NodeID 1]

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
              = evalRand (mutateNode genome1 [(Link (NodeID 0) (NodeID 1), InnovationID 0)]) (mkStdGen 0)
        resultGenome `shouldBe` genome0

    describe "mutateLink" $ do
      it "should create a new link random between unlinked nodes" $ do
        let (resultGenome, resultInnovations)
              = evalRand (mutateLink genome2 [(Link (NodeID 2) (NodeID 1), InnovationID 0) ]) (mkStdGen 0)
        (length $ genes resultGenome) `shouldBe` 1
        (innovationID $ head $ genes resultGenome) `shouldBe` (InnovationID 1)
        (link $ head $ genes resultGenome) `shouldBe` (Link (NodeID 0) (NodeID 1))
