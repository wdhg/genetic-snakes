module Neat.BreedingSpec where

import Neat
import Neat.TestUtils
import Test.Hspec

spec :: Spec
spec
  = describe "alignGenomes" $ do
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
