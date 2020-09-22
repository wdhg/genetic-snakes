module Neat.Breeding where

import Control.Monad        (foldM)
import Control.Monad.Random
import Data.List            (nub)
import Neat.Base

data InheritMode
  = InheritLeft
  | InheritRight
  | InheritBoth

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

inherit :: MonadRandom m => Genome -> Gene -> m Genome
inherit offspring gene
  = do
    if not $ enabled gene
       then do
         value <- getRandomR (0.0, 1.0) :: MonadRandom m => m Float
         let gene' = gene {enabled = value <= 0.25}
         return offspring {genes = genes offspring ++ [gene']}
       else return offspring {genes = genes offspring ++ [gene]}

inheritLeft :: MonadRandom m => Genome -> Alignment -> m Genome
inheritLeft offspring (LeftDisjoint gene)
  = inherit offspring gene
inheritLeft offspring (RightDisjoint _)
  = return offspring
inheritLeft offspring (Aligned gene _)
  = inherit offspring gene

inheritRight :: MonadRandom m => Genome -> Alignment -> m Genome
inheritRight offspring (LeftDisjoint _)
  = return offspring
inheritRight offspring (RightDisjoint gene)
  = inherit offspring gene
inheritRight offspring (Aligned _ gene)
  = inherit offspring gene

inheritBoth :: MonadRandom m => Genome -> Alignment -> m Genome
inheritBoth offspring alignment@(LeftDisjoint _)
  = inheritLeft offspring alignment
inheritBoth offspring alignment@(RightDisjoint _)
  = inheritRight offspring alignment
inheritBoth offspring alignment
  = do
    value <- getRandomR (0.0, 1.0) :: MonadRandom m => m Float
    if value < 0.5
       then inheritLeft offspring alignment
       else inheritRight offspring alignment

breed :: MonadRandom m => AssessedGenome -> AssessedGenome -> m Genome
breed (AssessedGenome genome0 fitness0) (AssessedGenome genome1 fitness1)
  | fitness0 > fitness1 = foldM inheritLeft (genome0 {genes = []}) aligned
  | fitness0 < fitness1 = foldM inheritRight (genome1 {genes = []}) aligned
  | otherwise           = foldM inheritBoth genome' aligned
    where
      (AlignedGenomes aligned) = alignGenomes genome0 genome1
      genome' = Genome
        { genes = []
        , inputs = inputs genome0
        , outputs = outputs genome0
        , hidden = nub $ (hidden genome0) ++ (hidden genome1)
        }

isDisjoint :: Alignment -> Bool
isDisjoint (Aligned _ _)
  = False
isDisjoint _
  = True

distance :: Genome -> Genome -> Float
distance genome0 genome1
  = let (AlignedGenomes aligned) = alignGenomes genome0 genome1
        disjoint = fromIntegral $ length $ filter isDisjoint aligned
        n = fromIntegral $ max (length $ genes genome0) (length $ genes genome1)
        getWeightDistance (Aligned g0 g1)
          = abs $ weight g0 - weight g1
        totalWeightDistance
          = (sum $ map getWeightDistance $ filter (not . isDisjoint) aligned)
     in (disjoint + totalWeightDistance * 0.4) / n
