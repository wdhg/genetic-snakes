module Neat.Base
  ( Node(..)
  , Link
  , Gene(..)
  , Genome
  , Organism(..)
  , SimulationState(..)
  ) where

import System.Random (StdGen)

data Node
  = Input Int
  | Output Int
  | Hidden Int
    deriving (Show, Eq, Ord)

type Link
  = (Node, Node)

data Gene
  = Gene
    { link         :: Link
    , weight       :: Float
    , enabled      :: Bool
    , innovationID :: Int
    }
    deriving (Show)

instance Eq Gene where
  gene1 == gene2
    = (innovationID gene1) == (innovationID gene2)

instance Ord Gene where
  compare gene1 gene2
    = compare (innovationID gene1) (innovationID gene2)

type Genome
  = [Gene]

data Organism
  = Organism
    { genome  :: Genome
    , inputs  :: Int
    , outputs :: Int
    , hidden  :: Int
    }
    deriving (Show)

data SimulationState
  = SimulationState
    { gen         :: StdGen
    , innovations :: [Link]
    }
    deriving (Show)
