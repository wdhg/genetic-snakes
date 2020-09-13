module Neat.Mutation.Gene where

import Neat.Mutation.Base
import Neat.Mutation.Utils

reenableGene :: Mutation Gene
reenableGene
 = return . setEnabledTo True

perturbGeneWeight :: Mutation Gene
perturbGeneWeight gene
  = do
    offset <- randomRState (-0.2, 0.2)
    return (gene {weight = weight gene + offset})

reassignGeneWeight :: Mutation Gene
reassignGeneWeight gene
  = do
    weight' <- randomRState (-2.0, 2.0)
    return (gene {weight = weight'})

mutateWeight :: Mutation Gene
mutateWeight
  = chanceMutations perturbChance perturbGeneWeight reassignGeneWeight
    where
      perturbChance = 0.9
