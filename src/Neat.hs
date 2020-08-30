module Neat where

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

data SimulationState
  = SimulationState
    { gen         :: StdGen
    , innovations :: Int
    }

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

data Genome
  = Genome
    { genes :: [Gene]
    , nodes :: Int
    }

type Mutation m
  = m -> State SimulationState m

chanceMutations :: Float -> Mutation a -> Mutation a -> Mutation a
chanceMutations chance mutationThen mutationElse mutable
  = do
    sim <- get
    let (value, gen') = randomR (0, 1) (gen sim) :: (Float, StdGen)
    put (sim {gen = gen'})
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
  = state $ \sim ->
      let (offset, gen')
            = randomR (-maxPerturbAmount, maxPerturbAmount) (gen sim)
       in (gene {weight = weight gene + offset}, sim {gen = gen'})

reassignGeneWeight :: Mutation Gene
reassignGeneWeight gene
  = state $ \sim ->
      let (weight', gen') = randomR (0, 1) (gen sim)
       in (gene {weight = weight'}, sim {gen = gen'})

mutateWeight :: Mutation Gene
mutateWeight
  = chanceMutations perturbChance perturbGeneWeight reassignGeneWeight

mutateWeights :: Mutation [Gene]
mutateWeights
  = chanceMutation mutateWeightsChance $ mapM mutateWeight

reenableGene :: Mutation Gene
reenableGene gene
  = return $ gene {enabled = True}

reenableGenes :: Mutation [Gene]
reenableGenes
  = mapM $ chanceMutation reenableChance reenableGene

mutateGenome :: Mutation Genome
mutateGenome genome
  = do
    genes' <- mutateWeights (genes genome) >>= reenableGenes
    return $ genome {genes = genes'}

alignGenes :: [Gene] -> [Gene] -> [(Maybe Gene, Maybe Gene)]
alignGenes genes1 []
  = map (\gene -> (Just gene, Nothing)) genes1
alignGenes [] genes2
  = map (\gene -> (Nothing, Just gene)) genes2
alignGenes genes1@(gene1:genes1') genes2@(gene2:genes2')
  | gene1 < gene2 = (Just gene1, Nothing) : alignGenes genes1' genes2
  | gene1 > gene2 = (Nothing, Just gene2) : alignGenes genes1 genes2'
  | otherwise     = (Just gene1, Just gene2) : alignGenes genes1' genes2'
