module Neat.Mutation.Gene where

import Neat.Mutation.Base
import Neat.Mutation.Utils

reenableGene :: Mutation Gene
reenableGene
 = return . setEnabledTo True

perturbGeneWeight :: Mutation Gene
perturbGeneWeight gene
  = state $ \sim ->
      let maxPerturbAmount = 0.2
          (offset, gen')
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
    where
      perturbChance = 0.9
