module Neat.Mutation.Base
  ( Chance
  , Mutation
  , module Neat.Base
  ) where

import Neat.Base

type Chance
  = Float

type Mutation m
  = m -> State SimulationState m
