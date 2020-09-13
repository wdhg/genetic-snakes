module Neat.Mutation.Utils where

import Neat.Base
import Neat.Mutation.Base

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ []
  = []
replaceAt 0 item (_ : items')
  = item : items'
replaceAt index item (item' : items)
  = item' : replaceAt (index - 1) item items

getIndex :: Eq a => a -> [a] -> Maybe Int
getIndex _ []
  = Nothing
getIndex item (item':items)
  | item == item' = Just 0
  | otherwise = succ <$> getIndex item items

setEnabledTo :: Bool -> Gene -> Gene
setEnabledTo isEnabled gene
  = gene {enabled = isEnabled}

chanceMutations :: Chance -> Mutation m -> Mutation m -> Mutation m
chanceMutations chance mutationThen mutationElse mutable
  = do
    sim <- get
    value <- randomRState (0, 1)
    if value <= chance
       then mutationThen mutable
       else mutationElse mutable

chanceMutation :: Chance -> Mutation m -> Mutation m
chanceMutation chance mutationThen
  = chanceMutations chance mutationThen (\m -> state $ \s -> (m, s))