module Neat.Breeding where

import Neat.Base

alignGenes :: Genome -> Genome -> [(Maybe Gene, Maybe Gene)]
alignGenes genome1 []
  = map (\gene -> (Just gene, Nothing)) genome1
alignGenes [] genome2
  = map (\gene -> (Nothing, Just gene)) genome2
alignGenes genome1@(gene1:genome1') genome2@(gene2:genome2')
  | gene1 > gene2 = (Just gene1, Nothing) : alignGenes genome1' genome2
  | gene1 < gene2 = (Nothing, Just gene2) : alignGenes genome1 genome2'
  | otherwise     = (Just gene1, Just gene2) : alignGenes genome1' genome2'
