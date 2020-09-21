module Neat.TestUtils where

import Neat.Base

g0, g1, g2 :: Gene
g0 = Gene (Link (Id 0) (Id 1)) 4.0 True (Id 0)
g1 = Gene (Link (Id 0) (Id 2)) 1.0 True (Id 1)
g2 = Gene (Link (Id 2) (Id 1)) 4.0 True (Id 2)

genome0, genome1 :: Genome
genome0
  = Genome [g2, g1, g0 {enabled = False}] [Id 0] [Id 1] [Id 2]
genome1
  = Genome [g0] [Id 0] [Id 1] []
genome2
  = Genome [] [Id 0] [] [Id 1]
