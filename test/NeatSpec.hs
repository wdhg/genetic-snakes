module NeatSpec where

import Control.Monad.Random
import Neat
import Test.Hspec

g0, g1, g2 :: Gene
g0 = Gene (Input 0) (Output 0) 0.0 True 0
g1 = Gene (Input 0) (Hidden 0) 1.0 True 1
g2 = Gene (Hidden 0) (Output 0) 4.0 True 2

genome0 :: Genome
genome0
  = [g0, g1, g2]

spec :: Spec
spec
  = describe "Neat" $ do
      describe "perturbWeights" $ do
        it "should adjust weights of genes in genome" $ do
          let result = evalRand (perturbWeights genome0) (mkStdGen 0)
          map weight result `shouldNotBe` map weight genome0
