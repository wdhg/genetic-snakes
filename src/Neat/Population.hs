module Neat.Population where

import Control.Monad.Random
import Neat.Base
import Neat.Breeding        (isCompatible)
import Neat.Mutation        (mutateLink)

data Species
  = Species
    { representative :: Genome
    , members        :: [Genome]
    }

data Population
  = Population
    { species     :: [Species]
    , innovations :: Innovations
    }

populate' :: MonadRandom m => ([Genome], Innovations) -> Genome -> m ([Genome], Innovations)
populate' (gs, innovations) template
  = do
    (g, innovations') <- mutateLink template innovations
    return (g:gs, innovations')

populate :: MonadRandom m => Int -> Genome -> m ([Genome], Innovations)
populate size template
  = foldM populate' ([], []) $ replicate size template

isMember :: Genome -> Species -> Bool
isMember g s
  = isCompatible g $ representative s

addMember :: Genome -> Species -> Species
addMember g s
  = s {members = g : members s}

speciate' :: Genome -> [Species] -> [Species]
speciate' g []
  = [Species g [g]]
speciate' g (s:s')
  | isMember g s = addMember g s : s'
  | otherwise    = s : speciate' g s'

speciate :: [Genome] -> [Species]
speciate
  = foldr speciate' []

newPopulation :: MonadRandom m => Int -> Genome -> m Population
newPopulation size template
  = do
    (genomes, innovations) <- populate size template
    return $ Population (speciate genomes) innovations
