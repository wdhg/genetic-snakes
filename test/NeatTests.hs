module NeatTests (tests) where

import Control.Monad.State
import Neat
import System.Random
import Test.HUnit

testState :: SimulationState
testState
  = SimulationState (mkStdGen 0)
    [ (Input 0, Output 0)
    , (Input 0, Hidden 0)
    , (Hidden 0, Output 0)
    ]

testEqual :: (Eq a, Show a) => String -> a -> a -> Test
testEqual msg expected actual
  = TestCase $ assertEqual msg expected actual

testBool :: String -> Bool -> Test
testBool msg result
  = TestCase $ assertBool msg result

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
    , testEqual "2 genes -> Gene in has correct innovationID" 3 $
        innovationID $ getGene (Hidden 0, Hidden 1) $ fst $ results !! 2
    , testEqual "2 genes -> Gene out has correct innovationID" 4 $
        innovationID $ getGene (Hidden 1, Output 0) $ fst $ results !! 2
    ]
      where
        getGene :: Link -> Organism -> Gene
        getGene link' organism
          = case filter (\g -> link g == link') $ genome organism of
              [gene] -> gene
              _      -> error $ "Gene with link " ++ show link' ++ " not found"
        results :: [(Organism, SimulationState)]
        results
          = map (\organism -> runState (mutateNode organism) testState)
            [ Organism [] 1 1 0
            , Organism [Gene (Input 0, Output 0) 0.5 True 0] 1 1 0
            , Organism [Gene (Hidden 0, Output 0) 0.5 True 1] 0 1 1
            ]

tests :: Test
tests
  = TestList
    [ "mutateNodes" ~: mutateNodeTests
    ]
