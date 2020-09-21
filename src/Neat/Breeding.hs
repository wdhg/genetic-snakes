module Neat.Breeding where

import Neat.Base

data Alignment
  = Aligned Gene Gene
  | LeftDisjoint Gene
  | RightDisjoint Gene
    deriving (Show, Eq)

data AlignedGenomes
  = AlignedGenomes [Alignment] deriving (Show, Eq)

alignGenomes' :: [Gene] -> [Gene] -> [Alignment]
alignGenomes' genes0 []
  = map LeftDisjoint genes0
alignGenomes' [] genes1
  = map RightDisjoint genes1
alignGenomes' (gene0:genes0) (gene1:genes1)
  | gene0 > gene1 = (LeftDisjoint gene0) : alignGenomes' genes0 (gene1:genes1)
  | gene0 < gene1 = (RightDisjoint gene1) : alignGenomes' (gene0:genes0) genes1
  | otherwise     = Aligned gene0 gene1 : alignGenomes' genes0 genes1

alignGenomes :: Genome -> Genome -> AlignedGenomes
alignGenomes genome0 genome1
  = AlignedGenomes $ alignGenomes' (genes genome0) (genes genome1)
