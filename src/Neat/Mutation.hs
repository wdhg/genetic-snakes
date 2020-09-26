module Neat.Mutation
  ( mutateWeights
  , mutateNode
  , mutateLink
  ) where

import Control.Monad.Random
import Control.Monad.State
import Neat.Base

-- simple rename for clarity
pick :: (Foldable t, MonadRandom m) => t a -> m (Maybe a)
pick = uniformMay

perturbWeight :: MonadRandom m => Gene -> m Gene
perturbWeight gene
  = do
    adjustment <- getRandomR (-1.0, 1.0)
    return gene {weight = weight gene + adjustment}

reassignWeight :: MonadRandom m => Gene -> m Gene
reassignWeight gene
  = do
    weight' <- getRandomR (-2.0, 2.0)
    return gene {weight = weight'}

mutateWeight :: MonadRandom m => Gene -> m Gene
mutateWeight gene
  = do
    value <- getRandomR (0.0, 1.0) :: MonadRandom m => m Double
    if value <= 0.9
       then perturbWeight gene
       else reassignWeight gene

mutateWeights :: MonadRandom m => Genome -> m Genome
mutateWeights genome
  = do
    genes' <- mapM mutateWeight $ genes genome
    return genome {genes = genes'}

addHiddenNode :: Genome -> (Id Node, Genome)
addHiddenNode genome
  = case hidden genome of
      []    -> let inputCount = length $ inputs genome
                   outputCount = length $ outputs genome
                   nodeCount = inputCount + outputCount
                in (Id nodeCount, genome {hidden = [Id nodeCount]})
      nodes -> (succ $ maximum nodes, genome)

trackInnovation :: Monad m => Link -> StateT Innovations m (Id Innovation)
trackInnovation link
  = do
    innovations <- get
    let newInnovID = Id $ length innovations
    put $ innovations ++ [(link, newInnovID)]
    return newInnovID

getInnovationID :: Monad m => Link -> StateT Innovations m (Id Innovation)
getInnovationID link
  = do
    innovations <- get
    case lookup link innovations of
      Just innovID -> return innovID
      Nothing      -> trackInnovation link

addGene :: Monad m => Link -> Float -> Genome -> StateT Innovations m Genome
addGene link weight genome
  = do
    innovID <- getInnovationID link
    let gene = Gene link weight True innovID
    return genome {genes = gene : genes genome}

disable :: Gene -> Genome -> Genome
disable gene genome
  = let (before, _ : after) = break (== gene) $ genes genome
     in genome {genes = before ++ [gene {enabled = False}] ++ after}

splitGeneInTwo :: Monad m => Gene -> Genome -> StateT Innovations m Genome
splitGeneInTwo gene genome
  = let (hiddenNode, genome') = addHiddenNode $ disable gene genome
        (Link inNode outNode) = link gene
        linkIn = Link inNode hiddenNode
        linkOut = Link hiddenNode outNode
     in addGene linkIn 1.0 genome' >>= addGene linkOut (weight gene)

mutateNode' :: MonadRandom m => Genome -> StateT Innovations m Genome
mutateNode' genome
  = do
    maybeGene <- pick $ genes genome
    case maybeGene of
      Nothing   -> return genome -- empty genome
      Just gene -> splitGeneInTwo gene genome

mutateNode :: MonadRandom m => Genome -> Innovations -> m (Genome, Innovations)
mutateNode genome innovations
  = runStateT (mutateNode' genome) innovations

getIncommingNodes :: Genome -> Id Node -> [Id Node]
getIncommingNodes genome node
  = let links = map link $ genes genome
        incommingLinks = filter (\l -> outNode l == node) links
        incommingNodes = map inNode incommingLinks
     in incommingNodes ++ concatMap (getIncommingNodes genome) incommingNodes

isNotCyclic :: Genome -> Link -> Bool
isNotCyclic genome (Link nodeIn nodeOut)
  = nodeIn /= nodeOut && nodeOut `notElem` (getIncommingNodes genome nodeIn)

isValidLink :: Genome -> Link -> Bool
isValidLink genome link'
  = let existingLinks = map link $ genes genome
     in isNotCyclic genome link' && link' `notElem` existingLinks

getUnlinkedNodePairs :: Genome -> [Link]
getUnlinkedNodePairs genome
  = let linkInputs = inputs genome ++ hidden genome
        linkOutputs = outputs genome ++ hidden genome
        allLinks = [Link i o | i <- linkInputs, o <- linkOutputs]
     in filter (isValidLink genome) allLinks

addRandomGene :: MonadRandom m => Link -> Genome -> StateT Innovations m Genome
addRandomGene link genome
  = do
    weight <- getRandomR (-2.0, 2.0)
    addGene link weight genome

mutateLink' :: MonadRandom m => Genome -> StateT Innovations m Genome
mutateLink' genome
  = do
    maybeLink <- pick $ getUnlinkedNodePairs genome
    case maybeLink of
      Nothing   -> return genome -- genome is fully connected already
      Just link -> addRandomGene link genome

mutateLink :: MonadRandom m => Genome -> Innovations -> m (Genome, Innovations)
mutateLink genome innovations
  = runStateT (mutateLink' genome) innovations
