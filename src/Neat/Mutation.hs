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

addHiddenNode :: Genome -> (NodeID, Genome)
addHiddenNode genome
  = case hidden genome of
      []    -> let nodeCount = (length $ inputs genome) + (length $ outputs genome)
                in (NodeID nodeCount, genome {hidden = [NodeID nodeCount]})
      nodes -> (succ $ maximum nodes, genome)

trackInnovation :: MonadRandom m => Link -> StateT Innovations m InnovationID
trackInnovation link
  = do
    innovations <- get
    let newInnovID = InnovationID $ length innovations
    put $ innovations ++ [(link, newInnovID)]
    return newInnovID

getInnovationID :: MonadRandom m => Link -> StateT Innovations m InnovationID
getInnovationID link
  = do
    innovations <- get
    case lookup link innovations of
      Just innovID -> return innovID
      Nothing      -> trackInnovation link

addLink :: MonadRandom m =>
  Link -> Float -> Genome -> StateT Innovations m Genome
addLink link weight genome
  = do
    innovID <- getInnovationID link
    let gene = Gene link weight True innovID
     in return genome {genes = gene : genes genome}

disable :: Gene -> Gene
disable gene
  = gene {enabled = False}

selectAndDisableRandomGene :: MonadRandom m => Genome -> m (Gene, Genome)
selectAndDisableRandomGene genome
  = do
    index <- getRandomR (0, (length $ genes genome) - 1)
    let (before, gene : after) = splitAt index $ genes genome
        genome' = genome {genes = before ++ [disable gene] ++ after}
    return (gene, genome')

mutateNode :: MonadRandom m => Genome -> Innovations -> m (Genome, Innovations)
mutateNode genome innovations
  = do
    (gene, genome') <- selectAndDisableRandomGene genome
    let (Link inNode outNode) = link gene
        (hiddenNode, genome'') = addHiddenNode genome'
        addInGene = addLink (Link inNode hiddenNode) 1.0
        addOutGene = addLink (Link hiddenNode outNode) (weight gene)
    runStateT (addInGene genome'' >>= addOutGene) innovations

getIncommingNodes :: Genome -> NodeID -> [NodeID]
getIncommingNodes genome node
  = let links = map link $ genes genome
        incommingLinks = filter (\l -> outNode l == node)
        incommingNodes = map inNode links
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
  = let allLinks = zipWith Link (inputs genome ++ hidden genome) (outputs genome ++ hidden genome)
     in filter (isValidLink genome) allLinks

addRandomLink :: MonadRandom m => Link -> Genome -> StateT Innovations m Genome
addRandomLink link genome
  = do
    weight <- getRandomR (-2.0, 2.0)
    addLink link weight genome

mutateLink :: MonadRandom m => Genome -> Innovations -> m (Genome, Innovations)
mutateLink genome innovations
  = do
    maybeLink <- pick $ getUnlinkedNodePairs genome
    case maybeLink of
      Nothing -> return (genome, innovations) -- genome is fully connected already
      Just link -> do
        weight <- getRandomR (-2.0, 2.0)
        runStateT (addLink link weight genome) innovations
