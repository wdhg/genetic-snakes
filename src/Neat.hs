module Neat where

import Control.Monad.State
import System.Random

-- config
-- weight mutation
mutateGeneWeightChance = 0.8 :: Float
perturbChance = 0.9          :: Float
maxPerturbAmount = 0.2       :: Float
-- reenable mutation
mutateGeneReenableChance = 0.1 :: Float

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

type Genome
  = [Gene]

data Organism
  = Organism Genome StdGen

chanceIfElse :: Monad m => Float -> StateT (a, StdGen) m () ->
  StateT (a, StdGen) m () -> StateT (a, StdGen) m ()
chanceIfElse chance actionThen actionElse
  = do
    (s, gen) <- get
    let (value, gen') = randomR (0, 1) gen
    put (s, gen')
    if value <= chance
      then actionThen
      else actionElse

chanceIf :: Monad m => Float -> StateT (a, StdGen) m () ->
  StateT (a, StdGen) m ()
chanceIf chance action
  = chanceIfElse chance action (state $ \s -> ((), s))

perturbGeneWeight :: Monad m => StateT (Gene, StdGen) m ()
perturbGeneWeight
  = state $ \(gene, gen) ->
      let (offset, gen') = randomR (-maxPerturbAmount, maxPerturbAmount) gen
      in ((), (gene {weight = weight gene + offset}, gen'))

reassignGeneWeight :: Monad m => StateT (Gene, StdGen) m ()
reassignGeneWeight
  = state $ \(gene, gen) ->
      let (weight', gen') = randomR (0, 1) gen
      in ((), (gene {weight = weight'}, gen'))

reenableGene :: Monad m => StateT (Gene, StdGen) m ()
reenableGene
  = do
    (gene, gen) <- get
    put (gene {enabled = True}, gen)

mutateGene :: Monad m => StateT (Gene, StdGen) m ()
mutateGene
  = do
    chanceIfElse perturbChance perturbGeneWeight reassignGeneWeight
    chanceIf mutateGeneReenableChance reenableGene

mutate :: Monad m => StateT () m Organism
mutate
  = undefined
