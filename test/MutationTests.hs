module MutationTests (tests) where

import Control.Monad.State
import Neat
import Neat.Mutation.Base
import System.Random
import Test.HUnit
import Utils

testState :: SimulationState
testState
  = SimulationState (mkStdGen 0)
    [ (Input 0, Output 0)
    , (Input 0, Hidden 0)
    , (Hidden 0, Output 0)
    , (Input 0, Hidden 1)
    ]

latestInnovation :: Int
latestInnovation
  = (length $ innovations testState) - 1

testOrganisms :: [Organism]
testOrganisms
  = [ Organism [] 1 1 0
    , Organism [Gene (Input 0, Output 0) 0.5 True 0] 1 1 0
    , Organism [Gene (Hidden 0, Output 0) 0.5 True 2] 0 1 1
    , Organism
      [ Gene (Input 0, Output 0) 0.5 True 0
      , Gene (Input 0, Hidden 0) 0.3 True 1
      ] 1 1 1
    , Organism
      [ Gene (Input 0, Output 0) 0.5 True 0
      ] 1 2 0
    , Organism [Gene (Input 0, Output 0) 0.5 False 0] 1 1 0
    ]

produceResults :: Mutation Organism -> [(Organism, SimulationState)]
produceResults mutation
  = map (\o -> runState (mutation o) testState) testOrganisms

getGene :: Link -> Organism -> Gene
getGene link' organism
  = case filter (\g -> link g == link') $ genome organism of
      [gene] -> gene
      _      -> error $ "Gene with link " ++ show link' ++ " not found"

mutateNodeTests :: Test
mutateNodeTests
  = TestList
    [ testEqual "No genes -> No new genes" [] $
        genome $ fst $ results !! 0
    , testEqual "No genes -> No new hidden nodes" 0 $
        hidden $ fst $ results !! 0
    , testEqual "No genes -> No innovations" (innovations testState) $
       innovations $ snd $ results !! 0
    , testEqual "1 gene -> 2 new genes" 3 $
       length $ genome $ fst $ results !! 1
    , testEqual "1 gene -> New hidden node" 1 $
       hidden $ fst $ results !! 1
    , testBool "1 gene -> Gene into new node" $
       (Input 0, Hidden 0) `elem` (map link $ genome $ fst $ results !! 1)
    , testBool "1 gene -> Gene out of new node" $
       (Hidden 0, Output 0) `elem` (map link $ genome $ fst $ results !! 1)
    , testEqual "1 gene -> Gene in has correct innovationID" 1 $
        innovationID $ getGene (Input 0, Hidden 0) $ fst $ results !! 1
    , testEqual "1 gene -> Gene out has correct innovationID" 2 $
        innovationID $ getGene (Hidden 0, Output 0) $ fst $ results !! 1
    , testBool "1 gene -> Old gene is disabled" $
        not $ enabled $ last $ genome $ fst $ results !! 1
    , testEqual "2 genes -> Gene in has correct innovationID" (latestInnovation + 1) $
        innovationID $ getGene (Hidden 0, Hidden 1) $ fst $ results !! 2
    , testEqual "2 genes -> Gene out has correct innovationID" (latestInnovation + 2) $
        innovationID $ getGene (Hidden 1, Output 0) $ fst $ results !! 2
    ]
      where
        results = produceResults mutateNode

mutateLinkTests :: Test
mutateLinkTests
  = TestList
    [ testEqual "No genes -> 1 new gene" 1 $
        length $ genome $ fst $ results !! 0
    , testBool "No genes -> Gene with correct input and output" $
        (Input 0, Output 0) `elem` (map link $ genome $ fst $ results !! 0)
    , testEqual "All genes taken -> No new genes" 1 $
        length $ genome $ fst $ results !! 1
    , testEqual "All genes taken -> No new innovations" (innovations testState) $
        innovations $ snd $ results !! 1
    , testEqual "New gene -> Correct innovationID" 2 $
        innovationID $ getGene (Hidden 0, Output 0) $ fst $ results !! 3
    , testEqual "New gene -> No new nodes" 0 $
        hidden $ fst $ results !! 0
    , testEqual "New innovation -> Correct innovationID" (latestInnovation + 1) $
        innovationID $ getGene (Input 0, Output 1) $ fst $ results !! 4
    ]
      where
        results = produceResults mutateLink

mutateWeightsTests :: Test
mutateWeightsTests
  = TestList
    [ testEqual "No genes -> No changes" [] $
        genome $ fst $ results !! 0
    , testBool "Some gene -> Weights are altered" $
        and $ zipWith (/=)
          (map weight $ genome $ testOrganisms !! 3)
          (map weight $ genome $ fst $ results !! 3)
    ]
      where
        results = produceResults mutateWeights

mutateReenableGenesTests :: Test
mutateReenableGenesTests
  = TestList
    [ testEqual "No genes -> No changes" [] $
        genome $ fst $ results !! 0
    , testBool "Disabled gene -> Gene reenabled" $
        enabled $ getGene (Input 0, Output 0) $ fst $ results !! 5
    ]
      where
        results = produceResults mutateReenableGenes

tests :: Test
tests
  = TestList
    [ "mutateNodes" ~: mutateNodeTests
    , "mutateLink" ~: mutateLinkTests
    , "mutateWeights" ~: mutateWeightsTests
    , "mutateReenableGenes" ~: mutateReenableGenesTests
    ]
