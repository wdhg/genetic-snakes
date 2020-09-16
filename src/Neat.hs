module Neat where

import Control.Monad.Random

data Node
  = Input Int
  | Output Int
  | Hidden Int

data Gene
  = Gene
    { inNode  :: Node
    , outNode :: Node
    , weight  :: Float
    , enabled :: Bool
    , innovID :: Int
    }

type Genome
  = [Gene]

perturbWeight :: RandomGen g => Gene -> Rand g Gene
perturbWeight gene
  = do
    adjustment <- getRandomR (-1.0, 1.0)
    return $ gene {weight = weight gene + adjustment}

perturbWeights :: RandomGen g => Genome -> Rand g Genome
perturbWeights
  = mapM perturbWeight
