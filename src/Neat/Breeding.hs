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

getOffspringBasis :: AssessedGenome -> AssessedGenome -> Genome
getOffspringBasis assessed0 assessed1
  = case getMode (fitness assessed0) (fitness assessed1) of
      InheritLeft  -> (genome assessed0) {genes = []}
      InheritRight -> (genome assessed1) {genes = []}
      InheritBoth  -> let hidden0 = hidden $ genome assessed0
                          hidden1 = hidden $ genome assessed1
                       in (genome assessed0)
                          { genes = []
                          , hidden = nub $ hidden0 ++ hidden1
                          }

breed :: MonadRandom m => AssessedGenome -> AssessedGenome -> m Genome
breed assessed0 assessed1
  = let mode = getMode (fitness assessed0) (fitness assessed1)
        alignments = alignGenomes (genome assessed0) (genome assessed1)
        offspring = getOffspringBasis assessed0 assessed1
     in foldM (inherit mode) offspring alignments
