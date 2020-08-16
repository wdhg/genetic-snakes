module Neat where

type Node
  = Int

-- (in node, out node, weight, enabled, innovation id)
type Gene
  = (Node, Node, Float, Bool, Int)

type Genome
  = [Gene]
