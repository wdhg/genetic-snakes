module Neat where

import Control.Monad       ((>=>))
import Control.Monad.State
import System.Random

-- config
-- weight mutation
mutateWeightsChance = 0.8 :: Float
perturbChance = 0.9       :: Float
maxPerturbAmount = 0.2    :: Float
-- reenable mutation
reenableChance = 0.1 :: Float

type Node
  = Int

data Gene
  = Gene
    { inNode       :: Node
    , outNode      :: Node
    , weight       :: Float
    , enabled      :: Bool
    , innovationID :: Int
    }
    deriving Show

instance Eq Gene where
  gene1 == gene2
    = (innovationID gene1) == (innovationID gene2)

instance Ord Gene where
  compare gene1 gene2
    = compare (innovationID gene1) (innovationID gene2)

type Genome
  = [Gene]

type Mutation m
  = m -> State StdGen m

chanceMutations :: Float -> Mutation a -> Mutation a -> Mutation a
chanceMutations chance mutationThen mutationElse mutable
  = do
    gen <- get
    let (value, gen') = randomR (0, 1) gen :: (Float, StdGen)
    put gen'
    if value <= chance
       then mutationThen mutable
       else mutationElse mutable

chanceMutation :: Float -> Mutation a -> Mutation a
chanceMutation chance mutationThen
  = chanceMutations chance mutationThen (\m -> state $ \s -> (m, s))

pureMutation :: Mutation a
pureMutation x
  = state $ \s -> (x, s)

perturbGeneWeight :: Mutation Gene
perturbGeneWeight gene
  = state $ \gen ->
      let (offset, gen') = randomR (-maxPerturbAmount, maxPerturbAmount) gen
      in (gene {weight = weight gene + offset}, gen')

reassignGeneWeight :: Mutation Gene
reassignGeneWeight gene
  = state $ \gen ->
      let (weight', gen') = randomR (0, 1) gen
      in (gene {weight = weight'}, gen')

mutateWeight :: Mutation Gene
mutateWeight
  = chanceMutations perturbChance perturbGeneWeight reassignGeneWeight

mutateWeights :: Mutation Genome
mutateWeights
  = chanceMutation mutateWeightsChance $ mapM mutateWeight

reenableGene :: Mutation Gene
reenableGene gene
  = return $ gene {enabled = True}

reenableGenes :: Mutation Genome
reenableGenes
  = mapM $ chanceMutation reenableChance reenableGene

mutateGenome :: Mutation Genome
mutateGenome
  = mutateWeights >=> reenableGenes

alignGenes :: Genome -> Genome -> [(Maybe Gene, Maybe Gene)]
alignGenes genome1 []
  = map (\gene -> (Just gene, Nothing)) genome1
alignGenes [] genome2
  = map (\gene -> (Nothing, Just gene)) genome2
alignGenes genome1@(gene1:genes1) genome2@(gene2:genes2)
  | gene1 < gene2 = (Just gene1, Nothing) : alignGenes genes1 genome2
  | gene1 > gene2 = (Nothing, Just gene2) : alignGenes genome1 genes2
  | otherwise     = (Just gene1, Just gene2) : alignGenes genes1 genes2
