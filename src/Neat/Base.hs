{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Neat.Base where

newtype InnovationID
  = InnovationID Int deriving (Show, Eq, Ord, Enum)

newtype NodeID
  = NodeID Int deriving (Show, Eq, Ord, Enum)

data Link
  = Link
    { inNode  :: NodeID
    , outNode :: NodeID
    } deriving (Show, Eq)

data Gene
  = Gene
    { link         :: Link
    , weight       :: Float
    , enabled      :: Bool
    , innovationID :: InnovationID
    }
    deriving (Show)

instance Eq Gene where
  gene0 == gene1
    = innovationID gene0 == innovationID gene1

data Genome
  = Genome
    { genes   :: [Gene]
    , inputs  :: [NodeID]
    , outputs :: [NodeID]
    , hidden  :: [NodeID]
    }
    deriving (Show, Eq)

type Innovations
  = [(Link, InnovationID)]
