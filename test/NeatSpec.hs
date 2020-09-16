module NeatSpec where

import Control.Monad.Random
import Control.Monad.State
import Neat
import Test.Hspec

g0, g1, g2 :: Gene
g0 = Gene (Node Input 0) (Node Output 1) 4.0 True 0
g1 = Gene (Node Input 0) (Node Hidden 2) 1.0 True 1
g2 = Gene (Node Hidden 2) (Node Output 1) 4.0 True 2

genome0, genome1 :: Genome
genome0
  = [g2, g1, g0 {enabled = False}]
genome1
  = [g0]

spec :: Spec
spec
  = do
    describe "perturbWeights" $ do
      it "should adjust weights of genes in genome" $ do
        let result = evalRand (perturbWeights genome0) (mkStdGen 0)
        map weight result `shouldNotBe` map weight genome0

    describe "mutateNode" $ do
      it "should replace a gene with two new genes" $ do
        let result = evalRand (evalStateT (mutateNode genome1) [(Node Input 0, Node Output 1)]) (mkStdGen 0)
        result `shouldBe` genome0
