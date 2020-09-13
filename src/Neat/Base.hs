module Neat.Base
  ( Node(..)
  , Link
  , Gene(..)
  , Genome
  , Organism(..)
  , SimulationState(..)
  , randomRState
  , module Control.Monad.State
  , module System.Random
  ) where

import Control.Monad.State
import System.Random

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

randomRState :: Random a => (a, a) -> State SimulationState a
randomRState range
  = do
      sim <- get
      let (value, gen') = randomR range $ gen sim
      put (sim {gen = gen'})
      return value
