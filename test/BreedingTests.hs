module BreedingTests (tests) where

import Neat
import Neat.Breeding
import Test.HUnit
import Utils

gX0, gX1, gX2 :: Gene
gX0 = Gene (Input 0, Output 0) 0.3 False 0
gX1 = Gene (Input 0, Hidden 0) 0.3 True 1
gX2 = Gene (Hidden 0, Output 0) 1.0 True 2

gY0, gY1, gY2 :: Gene
gY0 = Gene (Input 0, Output 0) 0.01 False 0
gY1 = Gene (Input 0, Hidden 0) 0.1 False 1
gY2 = Gene (Hidden 0, Output 0) 4.0 True 2

genomeX0, genomeY0 :: Genome
genomeX0
  = [gX2, gX1, gX0]
genomeY0
  = [gY2, gY1, gY0]

genomeX1, genomeY1 :: Genome
genomeX1
  = [gX0]
genomeY1
  = [gY2, gY1]

genomeX2, genomeY2 :: Genome
genomeX2
  = [gX2, gX0]
genomeY2
  = [gY2, gY0]

alignGenesTests :: Test
alignGenesTests
  = TestList
    [ testBool "No genes -> no output" $
        null $ alignGenes [] []
    , testEqual "Left genome has genes"
        (map (\x -> (Just x, Nothing)) genomeX0)
        (alignGenes genomeX0 [])
    , testEqual "Right genome has genes"
        (map (\y -> (Nothing, Just y)) genomeY0)
        (alignGenes [] genomeY0)
    , testEqual "Left and right genomes has genes"
        (zipWith (\x y -> (Just x, Just y)) genomeX0 genomeY0)
        (alignGenes genomeX0 genomeY0)
    , testEqual "No overlap"
        [(Nothing, Just gY2), (Nothing, Just gY1), (Just gX0, Nothing)]
        (alignGenes genomeX1 genomeY1)
    , testEqual "Some overlap"
        [(Just gX2, Just gY2), (Just gX1, Just gY1), (Just gX0, Nothing)]
        (alignGenes genomeX0 genomeY1)
    , testEqual "Some overlap with gaps"
        [(Just gX2, Just gY2), (Just gX1, Nothing), (Just gX0, Just gY0)]
        (alignGenes genomeX0 genomeY2)
    , testEqual "Shared gaps"
        [(Just gX2, Just gY2), (Just gX0, Just gY0)]
        (alignGenes genomeX2 genomeY2)
    ]

tests :: Test
tests
  = TestList
    [ "alignGenes" ~: alignGenesTests
    ]
