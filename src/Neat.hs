module Neat where

import System.Random

type Node
  = Int

-- (in node, out node, weight, enabled, innovation id)
type Gene
  = (Node, Node, Float, Bool, Int)

type Genome
  = [Gene]

data Organism
  = Organism Genome StdGen
