module Neat.BreedingSpec where

import Control.Monad.Random
import Neat
import Neat.TestUtils
import Test.Hspec

assessed0, assessed1 :: AssessedGenome
assessed0 = AssessedGenome genome0 (Fitness 1.0)
assessed1 = AssessedGenome genome1 (Fitness 0.5)

spec :: Spec
spec
  = do
    describe "alignGenomes" $ do
      it "aligns two equal genomes" $
        alignGenomes genome0 genome0 `shouldBe`
          ( AlignedGenomes
            [ Aligned g2 g2
            , Aligned g1 g1
            , Aligned g0 g0
            ])
      it "aligns two genomes of differing lengths" $
        alignGenomes genome0 genome1`shouldBe`
          ( AlignedGenomes
            [ LeftDisjoint g2
            , LeftDisjoint g1
            , Aligned g0 g0
            ])

    describe "breed" $ do
      it "should produce genomes with the same structure as the fitter parent" $ do
        let result = evalRand (breed assessed0 assessed1) (mkStdGen 0)
        genes result `shouldBe` genes genome0
        inputs result `shouldBe` [Id 0]
        outputs result `shouldBe` [Id 1]
        hidden result `shouldBe` [Id 2]
