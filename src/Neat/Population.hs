module Neat.Population where

import Control.Monad.Random
import Neat.Base
import Neat.Breeding        (isCompatible)
import Neat.Mutation        (mutateLink)

data Species
  = Species
    { representative :: Genome
    , members        :: [Organism]
    }

data Population
  = Population
    { species     :: [Species]
    , innovations :: Innovations
    }

populate' :: MonadRandom m => ([Organism], Innovations) -> Genome -> m ([Organism], Innovations)
populate' (os, innovations) template
  = do
    (g, innovations') <- mutateLink template innovations
    let o = Organism g (Fitness 0)
    return (o:os, innovations')

populate :: MonadRandom m => Int -> Genome -> m ([Organism], Innovations)
populate size template
  = foldM populate' ([], []) $ replicate size template

isMember :: Organism -> Species -> Bool
isMember o s
  = isCompatible (genome o) $ representative s

addMember :: Organism -> Species -> Species
addMember o s
  = s {members = o : members s}

speciate' :: Organism -> [Species] -> [Species]
speciate' o []
  = [Species (genome o) [o]]
speciate' o (s:s')
  | isMember o s = addMember o s : s'
  | otherwise    = s : speciate' o s'

speciate :: [Organism] -> [Species]
speciate
  = foldr speciate' []

nextGeneration :: MonadRandom m => Innovations -> Population -> m (Population, Innovations)
nextGeneration innovations population
  = undefined

newPopulation :: MonadRandom m => Int -> Genome -> m Population
newPopulation size template
  = do
    (organisms, innovations) <- populate size template
    return $ Population (speciate organisms) innovations
