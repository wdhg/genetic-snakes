{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Neat.Base where

data Node
data Innovation

newtype Id a
  = Id Int deriving (Show, Eq, Ord, Enum)

data Link
  = Link
    { inNode  :: Id Node
    , outNode :: Id Node
    } deriving (Show, Eq)

data Gene
  = Gene
    { link         :: Link
    , weight       :: Float
    , enabled      :: Bool
    , innovationID :: Id Innovation
    } deriving (Show)

instance Eq Gene where
  gene0 == gene1
    = innovationID gene0 == innovationID gene1

instance Ord Gene where
  compare gene0 gene1
    = compare (innovationID gene0) (innovationID gene1)

data Genome
  = Genome
    { genes   :: [Gene]
    , inputs  :: [Id Node]
    , outputs :: [Id Node]
    , hidden  :: [Id Node]
    } deriving (Show, Eq)

newtype Fitness
  = Fitness Float deriving (Show, Eq, Ord)

data Organism
  = Organism
    { genome  :: Genome
    , fitness :: Fitness
    }

type Innovations
  = [(Link, Id Innovation)]
