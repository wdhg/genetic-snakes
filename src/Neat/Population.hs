module Neat.Population where

import Neat.Base

data Species
  = Species
    { representative :: Genome
    , members        :: [Genome]
    }

newtype Population
  = Population [Species]
