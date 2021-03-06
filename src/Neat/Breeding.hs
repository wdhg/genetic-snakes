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

alignGenes :: [Gene] -> [Gene] -> [Alignment]
alignGenes genes0 []
  = map LeftDisjoint genes0
alignGenes [] genes1
  = map RightDisjoint genes1
alignGenes (gene0:genes0) (gene1:genes1)
  | gene0 > gene1 = (LeftDisjoint gene0) : alignGenes genes0 (gene1:genes1)
  | gene0 < gene1 = (RightDisjoint gene1) : alignGenes (gene0:genes0) genes1
  | otherwise     = Aligned gene0 gene1 : alignGenes genes0 genes1

alignGenomes :: Genome -> Genome -> [Alignment]
alignGenomes genome0 genome1
  = alignGenes (genes genome0) (genes genome1)

canInherit :: InheritMode -> Alignment -> Bool
canInherit InheritLeft (RightDisjoint _)
  = False
canInherit InheritRight (LeftDisjoint _)
  = False
canInherit _ _
  = True

getGene :: MonadRandom m => Alignment -> m Gene
getGene (LeftDisjoint gene)
  = return gene
getGene (RightDisjoint gene)
  = return gene
getGene (Aligned gene0 gene1)
  = do
    value <- getRandomR (0.0, 1.0) :: MonadRandom m => m Float
    if value < 0.5
       then return gene0
       else return gene1

eitherDisabled :: Alignment -> Bool
eitherDisabled (LeftDisjoint gene)
  = not $ enabled gene
eitherDisabled (RightDisjoint gene)
  = not $ enabled gene
eitherDisabled (Aligned gene0 gene1)
  = not $ enabled gene0 && enabled gene1

chanceDisable :: MonadRandom m => Gene -> m Gene
chanceDisable gene
  = do
    value <- getRandomR (0.0, 1.0) :: MonadRandom m => m Float
    return $ gene {enabled = value <= 0.25}

inherit :: MonadRandom m => InheritMode -> Genome -> Alignment -> m Genome
inherit mode offspring alignment
  | canInherit mode alignment = do
      gene <- getGene alignment
      if eitherDisabled alignment
         then do
           gene' <- chanceDisable gene
           return offspring {genes = genes offspring ++ [gene']}
        else return offspring {genes = genes offspring ++ [gene]}
  | otherwise = return offspring

getMode :: Fitness -> Fitness -> InheritMode
getMode fitness0 fitness1
  | fitness0 > fitness1 = InheritLeft
  | fitness0 < fitness1 = InheritRight
  | otherwise           = InheritBoth

getOffspringBasis :: Organism -> Organism -> Genome
getOffspringBasis organism0 organism1
  = case getMode (fitness organism0) (fitness organism1) of
      InheritLeft  -> (genome organism0) {genes = []}
      InheritRight -> (genome organism1) {genes = []}
      InheritBoth  -> let hidden0 = hidden $ genome organism0
                          hidden1 = hidden $ genome organism1
                       in (genome organism0)
                          { genes = []
                          , hidden = nub $ hidden0 ++ hidden1
                          }

breed :: MonadRandom m => Organism -> Organism -> m Genome
breed organism0 organism1
  = let mode = getMode (fitness organism0) (fitness organism1)
        alignments = alignGenomes (genome organism0) (genome organism1)
        offspring = getOffspringBasis organism0 organism1
     in foldM (inherit mode) offspring alignments

largerGenomeSize :: [Gene] -> [Gene] -> Float
largerGenomeSize genes0 genes1
  = fromIntegral $ max (length genes0) (length genes1)

countAlignments :: [Alignment] -> (Float, Float)
countAlignments []
  = (0.0, 0.0)
countAlignments (alignment:alignments)
  = let (alignedCount, disjointCount) = countAlignments alignments
     in case alignment of
          (Aligned _ _) -> (alignedCount + 1, disjointCount)
          _             -> (alignedCount, disjointCount + 1)

totalWeightDelta :: [Alignment] -> Float
totalWeightDelta []
  = 0.0
totalWeightDelta (alignment:alignments)
  = let remaining = totalWeightDelta alignments
     in case alignment of
          (Aligned g0 g1) -> (abs $ weight g0 - weight g1) + remaining
          _               -> remaining

compatibility :: Genome -> Genome -> Float
compatibility genome0 genome1
  = let alignments = alignGenomes genome0 genome1
        geneCount = largerGenomeSize (genes genome0) (genes genome1)
        (alignedCount, disjointCount) = countAlignments alignments
        meanWeightDelta = (totalWeightDelta alignments) / alignedCount
     in disjointCount / geneCount + meanWeightDelta * 0.4

isCompatible :: Genome -> Genome -> Bool
isCompatible genome0 genome1
  = compatibility genome0 genome1 <= 0.3
