module Neat.Mutation.Genome where

import Neat.Mutation.Base
import Neat.Mutation.Gene  (mutateWeight, reenableGene)
import Neat.Mutation.Utils

reenableGenes :: Mutation Genome
reenableGenes
  = mapM reenableGene

mutateWeights :: Mutation Genome
mutateWeights
  = mapM mutateWeight

mutateGenome :: Mutation Genome
mutateGenome
  = chanceMutation 0.8 mutateWeights >=> chanceMutation 0.1 reenableGenes
