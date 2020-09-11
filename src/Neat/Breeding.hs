module Neat.Breeding
  (
  ) where

import Neat.Base

alignGenes :: [Gene] -> [Gene] -> [(Maybe Gene, Maybe Gene)]
alignGenes genes1 []
  = map (\gene -> (Just gene, Nothing)) genes1
alignGenes [] genes2
  = map (\gene -> (Nothing, Just gene)) genes2
alignGenes genes1@(gene1:genes1') genes2@(gene2:genes2')
  | gene1 < gene2 = (Just gene1, Nothing) : alignGenes genes1' genes2
  | gene1 > gene2 = (Nothing, Just gene2) : alignGenes genes1 genes2'
  | otherwise     = (Just gene1, Just gene2) : alignGenes genes1' genes2'
